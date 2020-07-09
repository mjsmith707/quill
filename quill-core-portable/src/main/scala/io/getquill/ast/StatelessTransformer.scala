package io.getquill.ast

import scala.annotation.switch

trait StatelessTransformer {

  def apply(a: Ast): Ast = {
    //    a match {
    //      case e: Query               => apply(e)
    //      case e: Operation           => apply(e)
    //      case e: Action              => apply(e)
    //      case e: Value               => apply(e)
    //      case e: Assignment          => apply(e)
    //      case e: Function            => Function(e.params, apply(e.body))
    //      case e: Ident               => e
    //      case e: ExternalIdent       => e
    //      case e: Property            => apply(e)
    //      case e: Infix               => Infix(e.parts, e.params.map(apply), e.pure)
    //      case e: OptionOperation     => apply(e)
    //      case e: IterableOperation   => apply(e)
    //      case e: If                  => If(apply(e.condition), apply(e.`then`), apply(e.`else`))
    //      case e: Dynamic             => e
    //      case e: External            => e
    //      case e: QuotedReference     => e
    //      case e: Block               => Block(e.statements.map(apply))
    //      case e: Val                 => Val(e.name, apply(e.body))
    //      case o: Ordering            => o
    //      case e: OnConflict.Excluded => e
    //      case e: OnConflict.Existing => e
    //    }

    (a.tag: @switch) match {
      case 0 => apply(a.asInstanceOf[Query])
      case 1 => apply(a.asInstanceOf[Operation])
      case 2 => apply(a.asInstanceOf[Action])
      case 3 => apply(a.asInstanceOf[Value])
      case 4 => apply(a.asInstanceOf[Assignment])
      case 5 =>
        val e = a.asInstanceOf[Function]; Function(e.params, apply(e.body))
      case 6 => a
      case 7 => a
      case 8 => apply(a.asInstanceOf[Property])
      case 9 =>
        val e = a.asInstanceOf[Infix]; Infix(e.parts, e.params.map(apply), e.pure)
      case 10 => apply(a.asInstanceOf[OptionOperation])
      case 11 => apply(a.asInstanceOf[IterableOperation])
      case 12 =>
        val e = a.asInstanceOf[If]; If(apply(e.condition), apply(e.`then`), apply(e.`else`))
      case 13 => a
      case 14 => a
      case 15 => a
      case 16 =>
        val e = a.asInstanceOf[Block]; Block(e.statements.map(apply))
      case 17 =>
        val e = a.asInstanceOf[Val]; Val(e.name, apply(e.body))
      case 18 => a
      case 19 => a
      case 20 => a
    }
  }

  def apply(o: OptionOperation): OptionOperation = {
    //    o match {
    //      case e: OptionTableFlatMap => OptionTableFlatMap(apply(e.ast), e.alias, apply(e.body))
    //      case e: OptionTableMap     => OptionTableMap(apply(e.ast), e.alias, apply(e.body))
    //      case e: OptionTableExists  => OptionTableExists(apply(e.ast), e.alias, apply(e.body))
    //      case e: OptionTableForall  => OptionTableForall(apply(e.ast), e.alias, apply(e.body))
    //      case e: OptionFlatten      => OptionFlatten(apply(e.ast))
    //      case e: OptionGetOrElse    => OptionGetOrElse(apply(e.ast), apply(e.body))
    //      case e: OptionFlatMap      => OptionFlatMap(apply(e.ast), e.alias, apply(e.body))
    //      case e: OptionMap          => OptionMap(apply(e.ast), e.alias, apply(e.body))
    //      case e: OptionForall       => OptionForall(apply(e.ast), e.alias, apply(e.body))
    //      case e: OptionExists       => OptionExists(apply(e.ast), e.alias, apply(e.body))
    //      case e: OptionContains     => OptionContains(apply(e.ast), apply(e.body))
    //      case e: OptionIsEmpty      => OptionIsEmpty(apply(e.ast))
    //      case e: OptionNonEmpty     => OptionNonEmpty(apply(e.ast))
    //      case e: OptionIsDefined    => OptionIsDefined(apply(e.ast))
    //      case e: OptionSome         => OptionSome(apply(e.ast))
    //      case e: OptionApply        => OptionApply(apply(e.ast))
    //      case e: OptionOrNull       => OptionOrNull(apply(e.ast))
    //      case e: OptionGetOrNull    => OptionGetOrNull(apply(e.ast))
    //      case e: OptionNone.type    => e
    //    }

    (o.optionOperationTag: @switch) match {
      case 44 =>
        val e = o.asInstanceOf[OptionTableFlatMap]; OptionTableFlatMap(apply(e.ast), e.alias, apply(e.body))
      case 45 =>
        val e = o.asInstanceOf[OptionTableMap]; OptionTableMap(apply(e.ast), e.alias, apply(e.body))
      case 46 =>
        val e = o.asInstanceOf[OptionTableExists]; OptionTableExists(apply(e.ast), e.alias, apply(e.body))
      case 47 =>
        val e = o.asInstanceOf[OptionTableForall]; OptionTableForall(apply(e.ast), e.alias, apply(e.body))
      case 48 =>
        val e = o.asInstanceOf[OptionFlatten]; OptionFlatten(apply(e.ast))
      case 49 =>
        val e = o.asInstanceOf[OptionGetOrElse]; OptionGetOrElse(apply(e.ast), apply(e.body))
      case 50 =>
        val e = o.asInstanceOf[OptionFlatMap]; OptionFlatMap(apply(e.ast), e.alias, apply(e.body))
      case 51 =>
        val e = o.asInstanceOf[OptionMap]; OptionMap(apply(e.ast), e.alias, apply(e.body))
      case 52 =>
        val e = o.asInstanceOf[OptionForall]; OptionForall(apply(e.ast), e.alias, apply(e.body))
      case 53 =>
        val e = o.asInstanceOf[OptionExists]; OptionExists(apply(e.ast), e.alias, apply(e.body))
      case 54 =>
        val e = o.asInstanceOf[OptionContains]; OptionContains(apply(e.ast), apply(e.body))
      case 55 =>
        val e = o.asInstanceOf[OptionIsEmpty]; OptionIsEmpty(apply(e.ast))
      case 56 =>
        val e = o.asInstanceOf[OptionNonEmpty]; OptionNonEmpty(apply(e.ast))
      case 57 =>
        val e = o.asInstanceOf[OptionIsDefined]; OptionIsDefined(apply(e.ast))
      case 58 =>
        val e = o.asInstanceOf[OptionSome]; OptionSome(apply(e.ast))
      case 59 =>
        val e = o.asInstanceOf[OptionApply]; OptionApply(apply(e.ast))
      case 60 =>
        val e = o.asInstanceOf[OptionOrNull]; OptionOrNull(apply(e.ast))
      case 61 =>
        val e = o.asInstanceOf[OptionGetOrNull]; OptionGetOrNull(apply(e.ast))
      case 62 => o
    }
  }

  def apply(o: IterableOperation): IterableOperation = {
    //    o match {
    //      case e: MapContains  => MapContains(apply(e.ast), apply(e.body))
    //      case e: SetContains  => SetContains(apply(e.ast), apply(e.body))
    //      case e: ListContains => ListContains(apply(e.ast), apply(e.body))
    //    }

    (o.iterableOperationTag: @switch) match {
      case 63 =>
        val e = o.asInstanceOf[MapContains]; MapContains(apply(e.ast), apply(e.body))
      case 64 =>
        val e = o.asInstanceOf[SetContains]; SetContains(apply(e.ast), apply(e.body))
      case 65 => val e = o.asInstanceOf[ListContains]; ListContains(apply(e.ast), apply(e.body))
    }
  }

  def apply(q: Query): Query = {
    //    q match {
    //      case e: Entity      => e
    //      case e: Filter      => Filter(apply(e.query), e.alias, apply(e.body))
    //      case e: Map         => Map(apply(e.query), e.alias, apply(e.body))
    //      case e: FlatMap     => FlatMap(apply(e.query), e.alias, apply(e.body))
    //      case e: ConcatMap   => ConcatMap(apply(e.query), e.alias, apply(e.body))
    //      case e: SortBy      => SortBy(apply(e.query), e.alias, apply(e.criterias), e.ordering)
    //      case e: GroupBy     => GroupBy(apply(e.query), e.alias, apply(e.body))
    //      case e: Aggregation => Aggregation(e.operator, apply(e.ast))
    //      case e: Take        => Take(apply(e.query), apply(e.n))
    //      case e: Drop        => Drop(apply(e.query), apply(e.n))
    //      case e: Union       => Union(apply(e.a), apply(e.b))
    //      case e: UnionAll    => UnionAll(apply(e.a), apply(e.b))
    //      case e: Join        => Join(e.typ, apply(e.a), apply(e.b), e.aliasA, e.aliasB, apply(e.on))
    //      case e: FlatJoin    => FlatJoin(e.typ, apply(e.a), e.aliasA, apply(e.on))
    //      case e: Distinct    => Distinct(apply(e.a))
    //      case e: Nested      => Nested(apply(e.a))
    //    }

    (q.queryTag: @switch) match {
      case 21 => q
      case 22 =>
        val e = q.asInstanceOf[Filter]; Filter(apply(e.query), e.alias, apply(e.body))
      case 23 =>
        val e = q.asInstanceOf[Map]; Map(apply(e.query), e.alias, apply(e.body))
      case 24 =>
        val e = q.asInstanceOf[FlatMap]; FlatMap(apply(e.query), e.alias, apply(e.body))
      case 25 =>
        val e = q.asInstanceOf[ConcatMap]; ConcatMap(apply(e.query), e.alias, apply(e.body))
      case 26 =>
        val e = q.asInstanceOf[SortBy]; SortBy(apply(e.query), e.alias, apply(e.criterias), e.ordering)
      case 27 =>
        val e = q.asInstanceOf[GroupBy]; GroupBy(apply(e.query), e.alias, apply(e.body))
      case 28 =>
        val e = q.asInstanceOf[Aggregation]; Aggregation(e.operator, apply(e.ast))
      case 29 =>
        val e = q.asInstanceOf[Take]; Take(apply(e.query), apply(e.n))
      case 30 =>
        val e = q.asInstanceOf[Drop]; Drop(apply(e.query), apply(e.n))
      case 31 =>
        val e = q.asInstanceOf[Union]; Union(apply(e.a), apply(e.b))
      case 32 =>
        val e = q.asInstanceOf[UnionAll]; UnionAll(apply(e.a), apply(e.b))
      case 33 =>
        val e = q.asInstanceOf[Join]; Join(e.typ, apply(e.a), apply(e.b), e.aliasA, e.aliasB, apply(e.on))
      case 34 =>
        val e = q.asInstanceOf[FlatJoin]; FlatJoin(e.typ, apply(e.a), e.aliasA, apply(e.on))
      case 35 =>
        val e = q.asInstanceOf[Distinct]; Distinct(apply(e.a))
      case 36 => val e = q.asInstanceOf[Nested]; Nested(apply(e.a))
    }
  }

  def apply(e: Assignment): Assignment = {
    Assignment(e.alias, apply(e.property), apply(e.value))
  }

  def apply(e: Property): Property = {
    Property.Opinionated(apply(e.ast), e.name, e.renameable, e.visibility)
  }

  def apply(o: Operation): Operation = {
    //    o match {
    //      case e: UnaryOperation  => UnaryOperation(e.operator, apply(e.ast))
    //      case e: BinaryOperation => BinaryOperation(apply(e.a), e.operator, apply(e.b))
    //      case e: FunctionApply   => FunctionApply(apply(e.function), e.values.map(apply))
    //    }

    (o.operationTag: @switch) match {
      case 37 =>
        val e = o.asInstanceOf[UnaryOperation]; UnaryOperation(e.operator, apply(e.ast))
      case 38 =>
        val e = o.asInstanceOf[BinaryOperation]; BinaryOperation(apply(e.a), e.operator, apply(e.b))
      case 39 => val e = o.asInstanceOf[FunctionApply]; FunctionApply(apply(e.function), e.values.map(apply))
    }
  }

  def apply(v: Value): Value = {
    //    v match {
    //      case e: Constant       => e
    //      case e: NullValue.type => e
    //      case e: Tuple          => Tuple(e.values.map(apply))
    //      case e: CaseClass =>
    //        val (keys, values) = e.values.unzip
    //        CaseClass(keys.zip(values.map(apply)))
    //    }

    (v.valueTag: @switch) match {
      case 40 => v
      case 41 => v
      case 42 =>
        val e = v.asInstanceOf[Tuple]; Tuple(e.values.map(apply))
      case 43 =>
        val e = v.asInstanceOf[CaseClass]
        val (keys, values) = e.values.unzip
        CaseClass(keys.zip(values.map(apply)))
    }
  }

  def apply(a: Action): Action = {
    a match {
      case e: Update             => Update(apply(e.query), e.assignments.map(apply))
      case e: Insert             => Insert(apply(e.query), e.assignments.map(apply))
      case e: Delete             => Delete(apply(e.query))
      case e: Returning          => Returning(apply(e.action), e.alias, apply(e.property))
      case e: ReturningGenerated => ReturningGenerated(apply(e.action), e.alias, apply(e.property))
      case e: Foreach            => Foreach(apply(e.query), e.alias, apply(e.body))
      case e: OnConflict         => OnConflict(apply(e.insert), apply(e.target), apply(e.action))
    }

    (a.actionTag: @switch) match {
      case 66 =>
        val e = a.asInstanceOf[Update]; Update(apply(e.query), e.assignments.map(apply))
      case 67 =>
        val e = a.asInstanceOf[Insert]; Insert(apply(e.query), e.assignments.map(apply))
      case 68 =>
        val e = a.asInstanceOf[Delete]; Delete(apply(e.query))
      case 69 =>
        val e = a.asInstanceOf[Returning]; Returning(apply(e.action), e.alias, apply(e.property))
      case 70 =>
        val e = a.asInstanceOf[ReturningGenerated]; ReturningGenerated(apply(e.action), e.alias, apply(e.property))
      case 71 =>
        val e = a.asInstanceOf[Foreach]; Foreach(apply(e.query), e.alias, apply(e.body))
      case 72 => val e = a.asInstanceOf[OnConflict]; OnConflict(apply(e.insert), apply(e.target), apply(e.action))
    }
  }

  def apply(t: OnConflict.Target): OnConflict.Target =
    t match {
      case e: OnConflict.NoTarget.type => e
      case e: OnConflict.Properties    => OnConflict.Properties(e.props.map(apply))
    }

  def apply(a: OnConflict.Action): OnConflict.Action =
    a match {
      case e: OnConflict.Ignore.type => e
      case e: OnConflict.Update      => OnConflict.Update(e.assignments.map(apply))
    }

}
