package smithyu.codegen

import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.node.Node

type Hints = List[(ShapeId, Node)]

case class StaticMember(target: ShapeId, targetType: Type, hints: Hints)
case class Member(name: String, target: ShapeId, targetType: Type, hints: Hints)

enum Definition {
  case DOperation(
      input: StaticMember,
      errors: Option[StaticMember],
      output: StaticMember,
      streamedInput: Option[StaticMember],
      streamedOutput: Option[StaticMember]
  )
  case DProduct(shapeId: ShapeId, members: List[Member], hints: Hints)
  case DSum(shapeId: ShapeId, members: List[Member], hints: Hints)
  case DPrim(shapeId: ShapeId, tpe: PrimitiveType, hints: Hints)
  case DMap(
      shapeId: ShapeId,
      key: StaticMember,
      value: StaticMember,
      hints: Hints
  )
  case DList(shapeId: ShapeId, member: StaticMember, hints: Hints)
}

enum Type {
  case TPrim(prim: PrimitiveType)
  case TList(tpe: Type)
  case TMap(key: Type, value: Type)
  case TOption(tpe: Type)
  case TRef(fqn: ShapeId)
}

enum PrimitiveType {
  case PInteger
  case PLong
  case PFloat
  case PShort
  case PDouble
  case PBigInteger
  case PBigDecimal
  case PString
  case PBoolean
  case PBlob
  case PDocument
  case PTimestamp
}
