package smithyu.codegen

import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.node.*
import scala.jdk.CollectionConverters.*
import smithyu.codegen.Definition.DSum
import software.amazon.smithy.model.traits.DefaultTrait
import scala.jdk.OptionConverters.*

// scalafmt: {maxColumn = 120}

val quote               = "\""
def quoted(str: String) =
  StringBuilder().append(quote).append(str).append(quote).result()

extension (shapeId: ShapeId) {
  def render: String                                  = shapeId.getNamespace() + "." + shapeId.getName()
  def constructor                                     = shapeId.getName().capitalize
  def renderType                                      = shapeId.getNamespace() + "." + shapeId.getName().capitalize
  def schemaReference                                 = shapeId.renderType + ".schema"
  def renderLiteral: String                           = quoted(render)
  def addName: String                                 =
    s"|> withFullName ${quoted(shapeId.getNamespace())} ${quoted(shapeId.getName())}"
  def schemaDef(recursiveRoots: List[ShapeId]): Lines = Lines(
    if (recursiveRoots.isEmpty)
    then s"${shapeId.renderType}.schema = "
    else
      Lines(
        if (recursiveRoots == List(shapeId))
        then s"${shapeId.renderType}.schema = schemas.Schema.cyclic ${shapeId.renderType}.schemaY"
        else
          s"${shapeId.renderType}.schema = ${shapeId.renderType}.schemaY ${recursiveRoots.map(_.renderType + ".schema").mkString(" ")}"
        ,
        s"${shapeId.renderType}.schemaY ${(0 until recursiveRoots.size).map("y" + _).mkString(" ")} = "
      )
  )
}

extension (string: String) {
  def escaped       = string.replace("\\", "\\\\").replace(quote, "\\\"")
  def prefixIfUpper = if (string.headOption.exists(_.isUpper)) "_" + string else string
  def uncap         = if (string.isEmpty()) string else string.head.toLower + string.tail
}

extension (node: Node) {
  def renderLiteral: String = {
    quoted(Node.printJson(node).escaped)
  }
}

extension (number: Number) {

  def renderNat = {
    number.intValue().toString()
  }

  def renderInteger = {
    val int = number.longValue()
    if (int >= 0) s"+$int" else s"-$int"
  }

  def renderFloat = {
    val bd = BigDecimal(number.toString())
    if (bd.isValidLong) bd.toString() + ".0"
    else bd.toString()
  }
}

extension (tpe: Type) {
  def renderP =
    val rendered = tpe.render
    if (rendered.contains(" ")) s"($rendered)" else rendered

  def render: String = tpe match
    case Type.TPrim(prim) =>
      prim match
        case PrimitiveType.PInteger    => "Int"
        case PrimitiveType.PLong       => "Int"
        case PrimitiveType.PFloat      => "Float"
        case PrimitiveType.PDouble     => "Float"
        case PrimitiveType.PByte       => "Nat"
        case PrimitiveType.PShort      => "Int"
        case PrimitiveType.PBigInteger => "math.Integer"
        case PrimitiveType.PBigDecimal => "math.Decimal"
        case PrimitiveType.PString     => "Text"
        case PrimitiveType.PBoolean    => "Boolean"
        case PrimitiveType.PBlob       => "Bytes"
        case PrimitiveType.PDocument   => "Json"
        case PrimitiveType.PTimestamp  => "Instant"
        case PrimitiveType.PUnit       => "()"

    case Type.TList(t)         =>
      s"[${t.renderP}]"
    case Type.TMap(key, value) =>
      s"Map ${key.renderP} ${value.renderP}"
    case Type.TOption(tpe)     =>
      s"Optional ${tpe.renderP}"
    case Type.TRef(fqn)        => fqn.renderType

}

extension (hints: Hints) {
  def render: Lines = Lines {
    hints.map { case (shapeId, node) =>
      s"|> addJsonHint.unsafe ${shapeId.renderLiteral} ${node.renderLiteral}"
    }
  }
}

extension (member: NamedMember) {
  def renderField = s"${member.name}: ${member.targetType.render}"
  def renderAlt   =
    if member.targetType.isUnit
    then member.name.capitalize
    else s"${member.name.capitalize} ${member.targetType.renderP}"

  def renderSchema: Lines = Lines(
    s"${member.name}Schema = ",
    Lines.indent(
      // Taking care of the recursion
      if (member.rootIndexes.isEmpty)
      then s"${member.target.renderType}.schema"
      else s"(${member.target.renderType}.schemaY ${member.rootIndexes.map("y" + _).mkString(" ")})",
      Lines.indent(
        member.hints.render,
        if (member.targetType.isOption)
        then Lines("|> schemas.Schema.optional")
        else Lines.empty
      )
    )
  )

  def defaultNode: Option[Node] = member.hints.collectFirst {
    case (DefaultTrait.ID, node) if !node.isNullNode() => node
  }

  def renderDefault: Option[String] = {
    if (member.targetType.isOption) Some("None")
    else defaultNode.flatMap(_.accept(DefaultVisitor(member.targetType)))
  }

  def hasDefault: Boolean = renderDefault.nonEmpty
}

def renderOperation(service: ShapeId, operation: DOperation): Lines = {
  // Making a dedicated shapeId to cater to operations being used across several services.
  val serviceOperationShapeId =
    ShapeId.fromParts(service.renderType, operation.shapeId.getName())

  val errorShapeId = ShapeId.fromParts(operation.shapeId.renderType, "Error")
  val errorMembers = operation.errors.map(id =>
    NamedMember(
      id.getName() + "Case",
      id.getName(),
      id,
      Type.TRef(id),
      List.empty,
      List.empty,
      List.empty
    )
  )
  val errorUnion   = DSum(errorShapeId, List.empty, List.empty, errorMembers)
  Lines(
    if (errorMembers.isEmpty) then Lines.empty else renderDefinition(errorUnion),
    Lines.newline,
    serviceOperationShapeId.renderType + ".schema = ",
    Lines.indent(
      operation.streamedInput.map(_.named("streamedInput").renderSchema),
      operation.streamedOutput.map(_.named("streamedOutput").renderSchema),
      "schemas.OperationSchema.OperationSchema",
      Lines.indent(
        operation.input.target.renderType + ".schema",
        if (errorMembers.isEmpty) then "None" else s"(Some ${errorShapeId.renderType}.schema)",
        operation.output.target.renderType + ".schema",
        if operation.streamedInput.isDefined
        then "(Some streamedInputSchema)"
        else "None",
        if operation.streamedOutput.isDefined
        then "(Some streamedOutputSchema)"
        else "None",
        "Hints.empty",
        operation.shapeId.addName,
        operation.hints.render
      )
    )
  )

}

def renderDefinition(definition: Definition) = definition match
  case Definition.DPrim(shapeId, hints, tpe)                        =>
    val primSchema = tpe match
      case PrimitiveType.PBoolean    => "schemas.Schema.boolean"
      case PrimitiveType.PInteger    => "schemas.Schema.int"
      case PrimitiveType.PLong       => "schemas.Schema.int"
      case PrimitiveType.PFloat      => "schemas.Schema.float"
      case PrimitiveType.PDouble     => "schemas.Schema.float"
      case PrimitiveType.PByte       => "schemas.Schema.nat"
      case PrimitiveType.PShort      => "schemas.Schema.int"
      case PrimitiveType.PBigInteger => "schemas.Schema.bigInteger"
      case PrimitiveType.PBigDecimal => "schemas.Schema.bigDecimal"
      case PrimitiveType.PString     => "schemas.Schema.text"
      case PrimitiveType.PBlob       => "schemas.Schema.bytes"
      case PrimitiveType.PDocument   => "schemas.Schema.json"
      case PrimitiveType.PTimestamp  => "schemas.Schema.instant"
      case PrimitiveType.PUnit       => "schemas.Schema.unit"

    Lines(
      shapeId.renderType + ".schema = ",
      Lines.indent(
        primSchema,
        Lines.indent(
          shapeId.addName,
          hints.render
        )
      )
    )
  case Definition.DService(shapeId, hints, operations)              =>
    // AWS uses sometimes uses weird names for its service
    val maybeAWSSdkName = hints
      .find(_._1 == ShapeId.from("aws.api#service"))
      .map(_._2)
      .flatMap(_.asObjectNode().toScala)
      .map(_.expectStringMember("sdkId").getValue())

    val amendedId = maybeAWSSdkName match
      case Some(value) => ShapeId.fromParts(shapeId.getNamespace(), value)
      case None        => shapeId

    Lines(
      s"${amendedId.renderType}.hints =",
      Lines.indent(
        "schemas.Hints.empty",
        Lines.indent(
          s"|> schemas.Hints.member.add schemas.Name.schema (schemas.Name.Name (Some ${quoted(shapeId.getNamespace())}) ${quoted(shapeId.getName())})"
        )
      ),
      Lines.newline,
      operations.map(renderOperation(amendedId, _))
    )
  case Definition.DProduct(shapeId, hints, recursiveRoots, members) =>
    Lines(
      if members.nonEmpty
      then s"type ${shapeId.renderType} = {${members.map(_.renderField).mkString(", ")}}"
      else s"type ${shapeId.renderType} = ${shapeId.constructor}",
      // Minimal constructor
      s"${shapeId.renderType}.make ${members.filterNot(_.hasDefault).map(_.name).mkString(" ")} = ${shapeId.renderType}.${shapeId.constructor} ${members
          .map(m => m.renderDefault.getOrElse(m.name))
          .mkString(" ")}",
      // Schema
      shapeId.schemaDef(recursiveRoots),
      Lines.indent(
        members.map(_.renderSchema),
        "schemas.Schema.product",
        Lines.indent(
          "do",
          Lines.indent(
            members.map { member =>
              val modifier = member.renderDefault match
                case Some(value) => s"(Default $value)"
                case None        => "Required"
              s"""${member.name} = ProductSchematic.field ${member.name}Schema "${member.originalName}" ${shapeId.renderType}.${member.name} $modifier"""
            },
            s"ProductSchematic.absorb do ${shapeId.renderType}.${shapeId.constructor} ${members
                .map { m => m.name + "()" }
                .mkString(" ")}"
          ),
          shapeId.addName,
          hints.render
        )
      )
    )
  case Definition.DSum(shapeId, hints, recursiveRoots, members)     =>
    Lines(
      s"type ${shapeId.renderType} = ${members.map(_.renderAlt).mkString(" | ")}",
      shapeId.schemaDef(recursiveRoots),
      Lines.indent(
        members.map(_.renderSchema),
        "schemas.Schema.sum",
        Lines.indent(
          "do",
          Lines.indent(
            members.map { member =>
              if (member.targetType.isUnit)
              then
                s"""${member.name.prefixIfUpper} = SumSchematic.alt ${member.name}Schema "${member.originalName}" do ${member.name.capitalize}"""
              else
                s"""${member.name.prefixIfUpper} = SumSchematic.alt ${member.name}Schema "${member.originalName}" ${member.name.capitalize}"""
            },
            s"SumSchematic.absorb cases",
            Lines.indent(members.map { member =>
              if (member.targetType.isUnit)
              then s"${shapeId.renderType}.${member.name.capitalize} -> ${member.name.prefixIfUpper}()"
              else s"${shapeId.renderType}.${member.name.capitalize} value -> ${member.name.prefixIfUpper} value"
            })
          ),
          shapeId.addName,
          hints.render
        )
      )
    )
  case Definition.DMap(shapeId, hints, recursiveRoots, key, value)  =>
    Lines(
      shapeId.schemaDef(recursiveRoots),
      Lines.indent(
        key.named("key").renderSchema,
        value.named("value").renderSchema,
        "(Schema.map keySchema valueSchema)",
        Lines.indent(
          shapeId.addName,
          hints.render
        )
      )
    )
  case Definition.DList(shapeId, hints, recursiveRoots, member)     =>
    Lines(
      shapeId.schemaDef(recursiveRoots),
      Lines.indent(
        member.named("member").renderSchema,
        "memberSchema",
        Lines.indent(
          "|> Schema.list",
          shapeId.addName,
          hints.render
        )
      )
    )
  case Definition.DEnumeration(shapeId, hints, enumType, values)    =>
    val cases = enumType match
      case EnumType.ETString =>
        Lines(values.map { case (name, value) => (s"${shapeId.renderType}.$name -> \"$value\"") })
      case EnumType.ETInt    =>
        def signed(int: Int) = if (int >= 0) s"+$int" else s"-$int"
        Lines(values.map {
          case (name, value) => (s"${shapeId.renderType}.$name -> ${signed(value)}")
        })

    Lines(
      s"type ${shapeId.renderType} = ${values.map(_._1).mkString(" | ")}",
      shapeId.renderType + ".schema = ",
      Lines.indent(
        enumType match {
          case EnumType.ETString =>
            s"Schema.textEnum"
          case EnumType.ETInt    =>
            s"Schema.intEnum"
        },
        Lines.indent(
          s"[${values.map(_._1).mkString(", ")}]",
          "cases",
          cases.indent,
          shapeId.addName,
          hints.render
        )
      )
    )
  case Definition.Documentation(shapeId, direct, members)           =>
    Lines.empty
  // Lines(
  //   shapeId.renderType + ".documentationText = \"\"\"",
  //   Lines
  //     .indent(
  //       direct.lines().iterator().asScala.toList,
  //       members.map { case (memberName, text) =>
  //         val lines = text.lines().iterator().asScala.toList
  //         Lines(
  //           s"* $memberName: " ++ lines.head,
  //           lines.tail
  //         )
  //       }
  //     )
  //     .map(_.escaped),
  //   "\"\"\""
  // )
