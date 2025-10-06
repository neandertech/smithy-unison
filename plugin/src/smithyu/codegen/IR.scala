package smithyu.codegen

import software.amazon.smithy.model.shapes.ShapeId
import software.amazon.smithy.model.node.Node

type Hints = List[(ShapeId, Node)]

case class Member(
    target: ShapeId,
    targetType: Type,
    hints: Hints,
    recursiveRoots: List[ShapeId]
) {
  def named(str: String): NamedMember =
    NamedMember(str, target, targetType, hints, recursiveRoots)
}

case class NamedMember(
    name: String,
    target: ShapeId,
    targetType: Type,
    hints: Hints,
    recursiveRoots: List[ShapeId]
)

case class DOperation(
    shapeId: ShapeId,
    hints: Hints,
    input: Member,
    errors: List[ShapeId],
    output: Member,
    streamedInput: Option[Member],
    streamedOutput: Option[Member]
)

enum Definition {
  def shapeId: ShapeId

  case DService(
      shapeId: ShapeId,
      hints: Hints,
      operations: List[DOperation]
  )
  case DProduct(
      shapeId: ShapeId,
      hints: Hints,
      recursiveRoots: List[ShapeId],
      members: List[NamedMember]
  )
  case DSum(
      shapeId: ShapeId,
      hints: Hints,
      recursiveRoots: List[ShapeId],
      members: List[NamedMember]
  )
  case DPrim(
      shapeId: ShapeId,
      hints: Hints,
      tpe: PrimitiveType
  )
  case DMap(
      shapeId: ShapeId,
      hints: Hints,
      recursiveRoots: List[ShapeId],
      key: Member,
      value: Member
  )
  case DList(
      shapeId: ShapeId,
      hints: Hints,
      recursiveRoots: List[ShapeId],
      member: Member
  )

  case DEnumeration[A](
      shapeId: ShapeId,
      hints: Hints,
      enumType: EnumType[A],
      values: List[(String, A)]
  )

  case Documentation(
      shapeId: ShapeId,
      direct: String,
      members: List[(String, String)]
  )

}

enum EnumType[A] {
  case ETString extends EnumType[String]
  case ETInt    extends EnumType[Int]
}

enum Type {
  case TPrim(prim: PrimitiveType)
  case TList(tpe: Type)
  case TMap(key: Type, value: Type)
  case TOption(tpe: Type)
  case TRef(fqn: ShapeId)

  def isOption = this match
    case TOption(_) => true
    case _          => false

  def isUnit = this match
    case TPrim(PrimitiveType.PUnit) => true
    case _                          => false

}

enum PrimitiveType {
  case PInteger
  case PLong
  case PFloat
  case PByte
  case PShort
  case PDouble
  case PBigInteger
  case PBigDecimal
  case PString
  case PBoolean
  case PBlob
  case PDocument
  case PTimestamp
  case PUnit
}
