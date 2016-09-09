package org.camunda.feel.interpreter

import org.camunda.feel._
import org.joda.time.LocalDate

/**
 * @author Philipp Ossler
 */
case class Context(variables: Map[String, Any] = Map()) {

  def input: Val = apply(Context.inputKey)

  def apply(key: String): Val = variables.get(key) match {
    case None => ValError(s"no variable found for key '$key'")
    case Some(x : Val) => x
    case Some(x) => toVal(x)
  }

  private def toVal(x: Any): Val = x match {
    case x: Int => ValNumber(x)
    case x: Double => ValNumber(x)
    case x: Boolean => ValBoolean(x)
    case x: String => ValString(x)
    case x: Date => ValDate(x)
    case x: Time => ValTime(x)
    case x: Duration => ValDuration(x)
    case null => ValNull
    // extended types
    case x: java.util.Date => ValDate(LocalDate.fromDateFields(x))
    // unsupported values
    case None => ValError("no variable available")
    case _ => ValError(s"unsupported type of variable '$x'")
  }
  
  def ++(vars: Map[String, Any]) = Context(variables ++ vars)

}

object Context {

  val inputKey = "cellInput"

}
