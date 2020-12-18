package org.camunda.feel.impl.interpreter

import org.camunda.feel.context.{Context, FunctionProvider, VariableProvider}
import org.camunda.feel.syntaxtree.ValFunction

import scala.scalajs.js

/** A context that wraps the fields and methods of a given JVM object
  *
  * @param obj the JVM object to be wrapped
  */
case class ObjectContext(obj: Any) extends Context {
  def theObj = obj.asInstanceOf[js.Object]

  private def entries = js.Object.entries(theObj)

  val GetOrIs = "^(get|is)([A-Z])(\\w*)".r
  def normalizeFieldName(name: String) = name match {
    case GetOrIs(_, firstLetter, rest) => s"${firstLetter.toLowerCase()}$rest"
    case s => s
  }
  private def propertiesWithName(name: String) =
    js.Object.properties(theObj).filter(f => normalizeFieldName(f) == normalizeFieldName(name))
  private def entriesWithName(fieldName: String) = entries.filter(_._1 == fieldName)
  override val variableProvider = new VariableProvider {
    override def getVariable(name: String): Option[Any] = {

      println(s"Getting variable $name")
      val fields = js.Object.properties(theObj) find (field => field == name)

      /*
      fields.map(_.get(obj)) orElse {
        val methods = objClass.getMethods find (method => {
          val methodName = method.getName
          val returnType = method.getReturnType
          methodName == name ||
          methodName == getGetterName(name) ||
          ((returnType == java.lang.Boolean.TYPE ||
            returnType == classOf[java.lang.Boolean]) &&
            methodName == getBooleanGetterName(name))
        })

        methods.map(_.invoke(obj))
      }
      TODO
       */
      fields.map(f => obj.asInstanceOf[js.Dynamic].apply(f))
    }

    override def keys: Iterable[String] = js.Object.properties(obj.asInstanceOf[js.Any])
  }

  override val functionProvider = new FunctionProvider {
    override def getFunctions(name: String): List[ValFunction] = {
      val obj = ObjectContext.this.obj.asInstanceOf[js.Object]
      println(s"Getting functions with name $name for $obj")
      println(js.Object.properties(obj))

      propertiesWithName(name).map(f => ValFunction(List(), _ => entriesWithName(f).head._2)).toList
      /*
      obj.getClass.getMethods
        .find(method => {
          method.getName == name
        })
        .map(method => {
          val params = method.getParameters.map(param => param.getName).toList

          ValFunction(
            params,
            params => {

              val paramJavaObjects = params zip method.getParameterTypes map { case (obj, clazz) =>
                JavaClassMapper.asJavaObject(obj, clazz)
              }
              val result = method.invoke(obj, paramJavaObjects: _*)
              result
            }
          )
        })
        .toList
        TODO
       */
    }

    override def functionNames: Iterable[String] =
      js.Object.properties(obj.asInstanceOf[js.Any])
  }

}
