package smithyu.codegen

import software.amazon.smithy.model.Model
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import software.amazon.smithy.model.selector.PathFinder
import software.amazon.smithy.model.shapes.*
import software.amazon.smithy.model.traits.*

class SmithyToIR(model: Model) {

  val typeVisitor       = TypeVisitor()
  val definitionVisitor = DefinitionVisitor()

  val pathFinder = PathFinder.create(model)

  val types: Map[ShapeId, Type] = {
    model
      .shapes()
      .iterator()
      .asScala
      .flatMap(shape => shape.accept(typeVisitor).map(shape.getId() -> _))
      .toMap
  }

  // This represents all recursive roots for a given recursive shape.
  val recursiveRootsMap: Map[ShapeId, List[ShapeId]] = {
    // Gathering all recursive paths, as a set of sorted lists.
    val allPaths = model.shapes
      .iterator()
      .asScala
      .flatMap { shape =>
        val paths = pathFinder.search(shape, List(shape).asJava)
        paths.asScala
          .map(
            _.getShapes().asScala
              // Eliminating the member shapes, keepng only top-level shapes
              .filter(!_.isMemberShape())
              // Arbitrarily sorting by ShapeID
              .sortBy(_.getId().toString())
              .toList
          )
          .toSet
      }
      .toSet

    val shapeToRoot = for {
      recursivePaths <- allPaths
      // we're arbitrarily considering the head of the path as the recursive root
      recursiveRoot  <- recursivePaths.headOption.toList
      shape          <- recursivePaths
    } yield (shape.getId() -> recursiveRoot.getId())

    // This map represents all "recursive roots" for a given recursive shape
    val shapeToRoots = shapeToRoot.groupMap(_._1)(_._2).toMap
    shapeToRoots.mapValues(_.toList).toMap
  }

  def recursiveRoots(shapeId: ShapeId): List[ShapeId] =
    recursiveRootsMap.get(shapeId).getOrElse(List.empty)

  val definitions = model
    .shapes()
    .iterator()
    .asScala
    .flatMap(shape =>
      docs(shape).iterator ++ shape.accept(definitionVisitor).iterator
    )
    .toList
    .sortBy(_.shapeId.toString())

  // scalafmt: {maxColumn = 120}
  class DefinitionVisitor() extends ShapeVisitor[Option[Definition]] {

    private def acceptTrait(tr: Trait): Boolean = {
      val ignored = Set(
        DocumentationTrait.ID,
        TraitDefinition.ID,
        PrivateTrait.ID
      )
      !tr.isSynthetic() && !ignored(tr.toShapeId())
    }

    private def hints(shape: Shape): Hints                  =
      shape
        .getAllTraits()
        .asScala
        .filter { case (_, tr) => acceptTrait(tr) }
        .mapValues(_.toNode())
        .toList
        .sortBy(_._1.toString())
    private def primitive(shape: Shape): Option[Definition] =
      shape.accept(typeVisitor).collect { case Type.TPrim(tpe) =>
        Definition.DPrim(shape.getId(), hints(shape), tpe)
      }

    def member(m: MemberShape): Member = {
      val targetId    = m.getTarget()
      val memberHints = hints(m)
      val targetType  = types(m.getId())
      val memberRoots = recursiveRoots(targetId)
      val ownerRoots  = recursiveRoots(m.getContainer())
      val rootIndexes = memberRoots.map(r => ownerRoots.indexOf(r)).filter(_ >= 0)
      Member(targetId, targetType, memberHints, memberRoots, rootIndexes)
    }

    override def blobShape(shape: BlobShape): Option[Definition]             = primitive(shape)
    override def bigIntegerShape(shape: BigIntegerShape): Option[Definition] = primitive(shape)
    override def bigDecimalShape(shape: BigDecimalShape): Option[Definition] = primitive(shape)
    override def stringShape(shape: StringShape): Option[Definition]         = primitive(shape)
    override def shortShape(shape: ShortShape): Option[Definition]           = primitive(shape)
    override def integerShape(shape: IntegerShape): Option[Definition]       = primitive(shape)
    override def documentShape(shape: DocumentShape): Option[Definition]     = primitive(shape)
    override def byteShape(shape: ByteShape): Option[Definition]             = primitive(shape)
    override def timestampShape(shape: TimestampShape): Option[Definition]   = primitive(shape)
    override def doubleShape(shape: DoubleShape): Option[Definition]         = primitive(shape)
    override def booleanShape(shape: BooleanShape): Option[Definition]       = primitive(shape)
    override def floatShape(shape: FloatShape): Option[Definition]           = primitive(shape)
    override def longShape(shape: LongShape): Option[Definition]             = primitive(shape)

    override def enumShape(shape: EnumShape): Option[Definition] = Some {
      val values =
        shape.members().asScala.map(m => m.getMemberName() -> shape.getEnumValues().get(m.getMemberName())).toList
      Definition.DEnumeration(shape.getId(), hints(shape), EnumType.ETString, values)
    }

    override def intEnumShape(shape: IntEnumShape): Option[Definition] = Some {
      val values =
        shape.members().asScala.map(m => m.getMemberName() -> shape.getEnumValues().get(m.getMemberName()).toInt).toList
      Definition.DEnumeration(shape.getId(), hints(shape), EnumType.ETInt, values)
    }

    override def mapShape(shape: MapShape): Option[Definition] = Some {
      val roots = recursiveRoots(shape.toShapeId())
      Definition.DMap(shape.getId, hints(shape), roots, member(shape.getKey()), member(shape.getValue()))
    }

    override def listShape(shape: ListShape): Option[Definition] = Some {
      val roots = recursiveRoots(shape.toShapeId())
      Definition.DList(shape.getId, hints(shape), roots, member(shape.getMember()))
    }

    override def unionShape(shape: UnionShape): Option[Definition] = Some {
      val roots   = recursiveRoots(shape.toShapeId())
      val members = shape.members().asScala.map(s => member(s).named(s.getMemberName())).toList
      Definition.DSum(shape.getId, hints(shape), roots, members)
    }

    override def structureShape(shape: StructureShape): Option[Definition] =
      if (shape.hasTrait(classOf[UnitTypeTrait]))
      then
        Some {
          Definition.DPrim(shape.getId(), hints(shape), PrimitiveType.PUnit)
        }
      else
        Some {
          val roots   = recursiveRoots(shape.toShapeId())
          val members = shape.members().asScala.map(s => member(s).named(s.getMemberName())).toList
          Definition.DProduct(shape.getId, hints(shape), roots, members)
        }

    override def serviceShape(shape: ServiceShape): Option[Definition]     = None
    override def memberShape(shape: MemberShape): Option[Definition]       = None
    override def resourceShape(shape: ResourceShape): Option[Definition]   = None
    override def operationShape(shape: OperationShape): Option[Definition] = None

  }

  // scalafmt: {maxColumn = 120}
  class TypeVisitor() extends ShapeVisitor[Option[Type]] {

    private def primitive(primitiveType: PrimitiveType): Some[Type] = Some(
      Type.TPrim(primitiveType)
    )

    override def memberShape(shape: MemberShape): Option[Type] = {
      val target     = model.expectShape(shape.getTarget())
      val targetType = target.accept(this)
      val owner      = model.expectShape(shape.getContainer())
      if (owner.isStructureShape()) {
        if (shape.hasTrait(classOf[RequiredTrait])) targetType
        else if (shape.hasNonNullDefault()) targetType
        else targetType.map(Type.TOption(_))
      } else targetType
    }

    override def blobShape(shape: BlobShape): Option[Type]             = primitive(PrimitiveType.PBlob)
    override def bigIntegerShape(shape: BigIntegerShape): Option[Type] = primitive(PrimitiveType.PBigInteger)
    override def bigDecimalShape(shape: BigDecimalShape): Option[Type] = primitive(PrimitiveType.PBigDecimal)
    override def stringShape(shape: StringShape): Option[Type]         = primitive(PrimitiveType.PString)
    override def shortShape(shape: ShortShape): Option[Type]           = primitive(PrimitiveType.PShort)
    override def integerShape(shape: IntegerShape): Option[Type]       = primitive(PrimitiveType.PInteger)
    override def documentShape(shape: DocumentShape): Option[Type]     = primitive(PrimitiveType.PDocument)
    override def timestampShape(shape: TimestampShape): Option[Type]   = primitive(PrimitiveType.PTimestamp)
    override def doubleShape(shape: DoubleShape): Option[Type]         = primitive(PrimitiveType.PDouble)
    override def booleanShape(shape: BooleanShape): Option[Type]       = primitive(PrimitiveType.PBoolean)
    override def byteShape(shape: ByteShape): Option[Type]             = primitive(PrimitiveType.PByte)

    override def resourceShape(shape: ResourceShape): Option[Type]   = None
    override def operationShape(shape: OperationShape): Option[Type] = None
    override def mapShape(shape: MapShape): Option[Type]             = for {
      key   <- model.expectShape(shape.getKey().getId()).accept(this)
      value <- model.expectShape(shape.getValue().getId()).accept(this)
    } yield Type.TMap(key, value)

    override def listShape(shape: ListShape): Option[Type] =
      model.expectShape(shape.getMember().getId()).accept(this).map { member =>
        Type.TList(member)
      }

    override def unionShape(shape: UnionShape): Option[Type]     = Some(Type.TRef(shape.getId()))
    override def enumShape(shape: EnumShape): Option[Type]       = Some(Type.TRef(shape.getId()))
    override def intEnumShape(shape: IntEnumShape): Option[Type] = Some(Type.TRef(shape.getId()))

    override def structureShape(shape: StructureShape): Option[Type] =
      if (shape.getId() == ShapeId.fromParts("smithy.api", "Unit")) Some(Type.TPrim(PrimitiveType.PUnit))
      else Some(Type.TRef(shape.getId()))

    override def serviceShape(shape: ServiceShape): Option[Type] = None

    override def floatShape(shape: FloatShape): Option[Type] = primitive(PrimitiveType.PFloat)
    override def longShape(shape: LongShape): Option[Type]   = primitive(PrimitiveType.PLong)

  }

  def docs(shape: Shape): Option[Definition] = {
    val topDoc     = shape.getTrait(classOf[DocumentationTrait]).toScala.map(_.getValue())
    val memberDocs = shape
      .members()
      .asScala
      .flatMap { m =>
        m.getTrait(classOf[DocumentationTrait]).toScala.map(_.getValue()).map(m.getMemberName() -> _)
      }
      .toList
    if (shape.isResourceShape() || shape.isMemberShape()) None
    else if (topDoc.isEmpty && memberDocs.isEmpty) None
    else Some(Definition.Documentation(shape.getId, topDoc.getOrElse(""), memberDocs))
  }

}
