package smithyu.codegen

import software.amazon.smithy.model.node.*

class DefaultVisitor(tpe: Type) extends NodeVisitor[Option[String]] {

  override def numberNode(node: NumberNode): Option[String] =
    val value = node.getValue()
    tpe match
      case Type.TPrim(prim) =>
        prim match
          case PrimitiveType.PInteger    =>
            Some(value.renderInteger)
          case PrimitiveType.PLong       =>
            Some(value.renderInteger)
          case PrimitiveType.PFloat      =>
            Some(value.renderFloat)
          case PrimitiveType.PByte       =>
            Some(value.renderNat)
          case PrimitiveType.PShort      =>
            Some(value.renderInteger)
          case PrimitiveType.PDouble     =>
            Some(value.renderFloat)
          case PrimitiveType.PBigInteger =>
            Some(s"""math.Integer.fromText "${node.getValue}"""")
          case PrimitiveType.PBigDecimal =>
            Some(s"""math.Decimal.fromText "${node.getValue}"""")
          case PrimitiveType.PString     => None
          case PrimitiveType.PBoolean    => None
          case PrimitiveType.PBlob       => None
          case PrimitiveType.PDocument   => None
          case PrimitiveType.PTimestamp  => None
          case PrimitiveType.PUnit       => None
      case _                => None

  override def arrayNode(node: ArrayNode): Option[String] =
    tpe match
      case Type.TList(_) => Some("List.empty")
      case _             => None

  override def booleanNode(node: BooleanNode): Option[String] =
    tpe match
      case Type.TPrim(PrimitiveType.PBoolean) =>
        Some(node.getValue().toString())
      case _                                  => None

  override def stringNode(node: StringNode): Option[String] =
    tpe match
      case Type.TPrim(PrimitiveType.PString) =>
        Some(quoted(node.getValue().escaped))
      case _                                 => None

  override def nullNode(node: NullNode): Option[String] = None

  override def objectNode(node: ObjectNode): Option[String] =
    tpe match
      case Type.TMap(_, _) => Some("Map.empty")
      case _               => None
}
