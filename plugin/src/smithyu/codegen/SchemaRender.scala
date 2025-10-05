package smithyu.codegen

import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.node.Node
import software.amazon.smithy.model.node.NodeVisitor
import software.amazon.smithy.model.node.NullNode
import software.amazon.smithy.model.node.BooleanNode
import software.amazon.smithy.model.node.StringNode
import software.amazon.smithy.model.node.ArrayNode
import software.amazon.smithy.model.node.NumberNode
import software.amazon.smithy.model.node.ObjectNode

val quote               = "\""
def quoted(str: String) =
  StringBuilder().append(quote).append(str).append(quote).result()

extension (shapeId: ShapeId) {
  def render: String        = shapeId.getNamespace() + "." + shapeId.getName()
  def constructor           = shapeId.getName().capitalize
  def renderType            = shapeId.getNamespace() + "." + shapeId.getName().capitalize
  def schemaReference       = shapeId.renderType + ".schema"
  def renderLiteral: String = quoted(render)
  def addName: String       =
    s"|> withFullName ${quoted(shapeId.getNamespace())} ${quoted(shapeId.getName())}"
}

extension (node: Node) {
  def renderLiteral: String = {
    quoted(Node.printJson(node).replace("\\", "\\\\").replace(quote, "\\\""))
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
      s"|> schemas.Schema.addJsonHint.unsafe ${shapeId.renderLiteral} ${node.renderLiteral}"
    }
  }
}

extension (member: NamedMember) {
  def renderField = s"${member.name}: ${member.targetType.render}"
  def renderAlt   =
    if member.targetType.isUnit
    then member.name.capitalize
    else s"${member.name.capitalize} ${member.targetType.render}"

  def renderSchema: Lines = Lines(
    s"${member.name}Schema = ",
    Lines.indent(
      s"${member.target.renderType}.schema",
      Lines.indent(
        member.hints.render,
        if (member.targetType.isOption)
        then Lines("|> schemas.Schema.optional")
        else Lines.empty
      )
    )
  )
}

def renderDefinition(definition: Definition) = definition match
  case Definition.DPrim(shapeId, hints, tpe)                        =>
    val primSchema = tpe match
      case PrimitiveType.PInteger    => "schemas.Schema.int"
      case PrimitiveType.PLong       => "schemas.Schema.int"
      case PrimitiveType.PFloat      => "schemas.Schema.float"
      case PrimitiveType.PDouble     => "schemas.Schema.float"
      case PrimitiveType.PByte       => "schemas.Schema.nat"
      case PrimitiveType.PShort      => "schemas.Schema.int"
      case PrimitiveType.PBigInteger => "schemas.Schema.bigInteger"
      case PrimitiveType.PBigDecimal => "schemas.Schema.bigDecimal"
      case PrimitiveType.PString     => "schemas.Schema.text"
      case PrimitiveType.PBoolean    => "schemas.Schema.boolean"
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
    Lines.empty
  case Definition.DProduct(shapeId, hints, recursiveRoots, members) =>
    Lines(
      if members.nonEmpty
      then
        s"type ${shapeId.renderType} = {${members.map(_.renderField).mkString(", ")}}"
      else s"type ${shapeId.renderType} = ${shapeId.constructor}",
      s"${shapeId.renderType}.schema = ",
      Lines.indent(
        members.map(_.renderSchema),
        "schemas.Schema.product",
        Lines.indent(
          "do",
          Lines.indent(
            members.map { member =>
              val modifier =
                if member.targetType.isOption
                then "(Default None)"
                else "Required"
              s"""${member.name} = ProductSchematic.field ${member.name}Schema "${member.name}" ${shapeId.renderType}.${member.name} $modifier"""
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
      s"${shapeId.renderType}.schema = ",
      Lines.indent(
        members.map(_.renderSchema),
        "schemas.Schema.sum",
        Lines.indent(
          "do",
          Lines.indent(
            members.map { member =>
              if (member.targetType.isUnit)
              then
                s"""${member.name} = SumSchematic.alt ${member.name}Schema "${member.name}" do ${member.name.capitalize}"""
              else
                s"""${member.name} = SumSchematic.alt ${member.name}Schema "${member.name}" ${member.name.capitalize}"""
            },
            s"SumSchematic.absorb cases",
            Lines.indent(members.map { member =>
              if (member.targetType.isUnit)
              then s"${member.name.capitalize} -> ${member.name}()"
              else s"${member.name.capitalize} value -> ${member.name} value"
            })
          ),
          shapeId.addName,
          hints.render
        )
      )
    )
  case Definition.DMap(shapeId, hints, recursiveRoots, key, value)  =>
    Lines(
      shapeId.renderType + ".schema = ",
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
      shapeId.renderType + ".schema = ",
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
