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
  def renderType            = shapeId.getNamespace() + "." + shapeId.getName().capitalize
  def renderLiteral: String = quoted(render)
  def addName: String       =
    s"|> addHint Name.schema (Name (Some ${quoted(shapeId.getNamespace())}) ${quoted(shapeId.getName())})"
}

extension (node: Node) {
  def renderLiteral: String = {
    quoted(Node.printJson(node).replace("\\", "\\\\").replace(quote, "\\\""))
  }
}

extension (hints: Hints) {
  def render: Lines = Lines {
    hints.map { case (shapeId, node) =>
      s"|> schemas.Schema.addJsonHint.unsafe ${shapeId.renderLiteral} ${node.renderLiteral}"
    }
  }

}

def renderSchema(definition: Definition) = definition match
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
    Lines.empty
  case Definition.DSum(shapeId, hints, recursiveRoots, members)     =>
    Lines.empty
  case Definition.DMap(shapeId, hints, recursiveRoots, key, value)  =>
    Lines.empty
  case Definition.DList(shapeId, hints, recursiveRoots, member)     =>
    Lines.empty
