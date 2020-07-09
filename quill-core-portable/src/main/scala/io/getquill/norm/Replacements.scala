package io.getquill.norm

import java.util

import io.getquill.ast.Ast

import scala.collection.JavaConverters._
import scala.collection.immutable.Map

/**
 * When doing beta reductions, the Opinions of AST elements need to be set to their
 * neutral positions.
 *
 * For example, the `Property` AST element has a field
 * called `renameable` which dicatates whether to use a `NamingStrategy`
 * during tokenization in `SqlIdiom` (and other idioms) or not. Since this property
 * only does things after Normalization, it should be completely transparent to
 * beta reduction (all AST Opinion's have the same behavior).
 * This is why we need to automatically set the `renameable` field to
 * a pre-defined value every time `Property` is looked up. This is
 * done via the `Ast.neutralize` method.
 */
case class Replacements(map: collection.Map[Ast, Ast]) {

  private lazy val neutralized = {
    val m = new util.HashMap[Ast, Ast]()
    map.foreach {
      case (k, v) =>
        m.put(k.neutralize, v)
    }
    m.asScala
  }

  /** First transformed object to meet criteria **/
  def apply(key: Ast): Ast = {
    neutralized(key.neutralize)
  }

  /** First transformed object to meet criteria or none of none meets **/
  def get(key: Ast): Option[Ast] = {
    neutralized.get(key.neutralize)
  }

  /** Does the map contain a normalized version of the view you want to see */
  def contains(key: Ast): Boolean = {
    neutralized.contains(key.neutralize)
  }

  def ++(otherMap: collection.Map[Ast, Ast]): Replacements =
    Replacements(map ++ otherMap)

  def -(key: Ast): Replacements = {
    val newMap = map.toList.filterNot { case (k, v) => k.neutralize == key.neutralize }.toMap
    Replacements(newMap)
  }
}

object Replacements {
  def empty: Replacements =
    Replacements(Map())
}