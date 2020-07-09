package io.getquill.norm

import io.getquill.ast._

import scala.annotation.switch

object NormalizeNestedStructures {

  def unapply(q: Query): Option[Query] = {
    //    q match {
    //      case e: Entity          => None
    //      case Map(a, b, c)       => apply(a, c)(Map(_, b, _))
    //      case FlatMap(a, b, c)   => apply(a, c)(FlatMap(_, b, _))
    //      case ConcatMap(a, b, c) => apply(a, c)(ConcatMap(_, b, _))
    //      case Filter(a, b, c)    => apply(a, c)(Filter(_, b, _))
    //      case SortBy(a, b, c, d) => apply(a, c)(SortBy(_, b, _, d))
    //      case GroupBy(a, b, c)   => apply(a, c)(GroupBy(_, b, _))
    //      case Aggregation(a, b)  => apply(b)(Aggregation(a, _))
    //      case Take(a, b)         => apply(a, b)(Take)
    //      case Drop(a, b)         => apply(a, b)(Drop)
    //      case Union(a, b)        => apply(a, b)(Union)
    //      case UnionAll(a, b)     => apply(a, b)(UnionAll)
    //      case Distinct(a)        => apply(a)(Distinct)
    //      case Nested(a)          => apply(a)(Nested)
    //      case FlatJoin(t, a, iA, on) =>
    //        (Normalize(a), Normalize(on)) match {
    //          case (`a`, `on`) => None
    //          case (a, on)     => Some(FlatJoin(t, a, iA, on))
    //        }
    //      case Join(t, a, b, iA, iB, on) =>
    //        (Normalize(a), Normalize(b), Normalize(on)) match {
    //          case (`a`, `b`, `on`) => None
    //          case (a, b, on)       => Some(Join(t, a, b, iA, iB, on))
    //        }
    //    }

    (q.queryTag: @switch) match {
      case 21 => None
      case 23 =>
        val e = q.asInstanceOf[Map]; apply(e.query, e.body)(Map(_, e.alias, _))
      case 24 =>
        val e = q.asInstanceOf[FlatMap]; apply(e.query, e.body)(FlatMap(_, e.alias, _))
      case 25 =>
        val e = q.asInstanceOf[ConcatMap]; apply(e.query, e.body)(ConcatMap(_, e.alias, _))
      case 22 =>
        val e = q.asInstanceOf[Filter]; apply(e.query, e.body)(Filter(_, e.alias, _))
      case 26 =>
        val e = q.asInstanceOf[SortBy]; apply(e.query, e.criterias)(SortBy(_, e.alias, _, e.ordering))
      case 27 =>
        val e = q.asInstanceOf[GroupBy]; apply(e.query, e.body)(GroupBy(_, e.alias, _))
      case 28 =>
        val e = q.asInstanceOf[Aggregation]; apply(e.ast)(Aggregation(e.operator, _))
      case 29 =>
        val e = q.asInstanceOf[Take]; apply(e.query, e.n)(Take)
      case 30 =>
        val e = q.asInstanceOf[Drop]; apply(e.query, e.n)(Drop)
      case 31 =>
        val e = q.asInstanceOf[Union]; apply(e.a, e.b)(Union)
      case 32 =>
        val e = q.asInstanceOf[UnionAll]; apply(e.a, e.b)(UnionAll)
      case 35 =>
        val e = q.asInstanceOf[Distinct]; apply(e.a)(Distinct)
      case 36 =>
        val e = q.asInstanceOf[Nested]; apply(e.a)(Nested)
      case 34 =>
        val e = q.asInstanceOf[FlatJoin]
        (Normalize(e.a), Normalize(e.on)) match {
          case (e.`a`, e.`on`) => None
          case (a, on)         => Some(FlatJoin(e.typ, a, e.aliasA, on))
        }
      case 33 =>
        val e = q.asInstanceOf[Join]
        (Normalize(e.a), Normalize(e.b), Normalize(e.on)) match {
          case (e.`a`, e.`b`, e.`on`) => None
          case (a, b, on)             => Some(Join(e.typ, a, b, e.aliasA, e.aliasB, on))
        }
    }
  }

  private def apply(a: Ast)(f: Ast => Query) =
    (Normalize(a)) match {
      case (`a`) => None
      case (a)   => Some(f(a))
    }

  private def apply(a: Ast, b: Ast)(f: (Ast, Ast) => Query) =
    (Normalize(a), Normalize(b)) match {
      case (`a`, `b`) => None
      case (a, b)     => Some(f(a, b))
    }
}
