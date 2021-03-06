// import the FEEL engine library
import $ivy.`org.camunda.feel:feel-engine:1.12.4`, org.camunda.feel._

// import a logging library
import $ivy.`org.apache.logging.log4j:log4j-slf4j-impl:2.14.0`, org.apache.logging.log4j.core.config.Configurator, org.apache.logging.log4j.Level
// and enable the logging to print warnings
Configurator.setRootLevel(Level.WARN)

// initialize the FEEL engine
val feelEngine = new FeelEngine() 

// define a shortcut function for evaluation
def feel(expression: String, context: Map[String, Any] = Map.empty) {
  feelEngine.evalExpression(expression, context) match {
    case Right(result) => println(fansi.Color.LightGreen(s"> $result"))
    case Left(failure) => println(fansi.Color.LightRed(s"> $failure"))  
  }
}

println(fansi.Color.LightBlue("===== FEEL Engine REPL ======"))
println(fansi.Color.LightBlue("""> Evaluate FEEL expressions using 'feel("1 + 3")'"""))
println(fansi.Color.LightBlue("""> Provide variables using 'feel("x + 3", Map("x" -> 2))'"""))

// usage: amm --predef feel-repl.sc
