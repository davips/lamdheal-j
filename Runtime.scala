/**
 * Not to use directly.
 * This is for runtime only.
 */
object Runtime extends App {

   import scala.reflect.runtime.universe._
   class L[A: TypeTag](val l: List[A]) {
      override def toString = typeOf[A] match {
         case t if t =:= typeOf[Char] => l.mkString
//         case t if t =:= typeOf[List] => l.mkString
         case _ => "[" + l.mkString(", ") + "]"
      }

      def apply[U: TypeTag](f: (A) => U) = { new L[U](l map f) }
   }

   def interpret(typ: String)(lst: List[Char]) = {
      val str = "val resulting_value = " + lst.mkString
      import java.io.{FileOutputStream, PrintStream}
      val out = new PrintStream(new FileOutputStream("/dev/null"))
      val flusher = new java.io.PrintWriter(out)
      val interpret = {
         val settings = new scala.tools.nsc.GenericRunnerSettings(println)
         settings.usejavacp.value = true
         new scala.tools.nsc.interpreter.IMain(settings, flusher)
      }
      interpret.interpret(str)
      val resulting_type = interpret.typeOfTerm("resulting_value")
      resulting_type.toString() match {
         case "String" => if (typ != "[cha]") throw new Exception("It was expected a '" + typ + "', not a string from Scala code.")
         case "Double" => if (typ != "num") throw new Exception("It was expected a '" + typ + "', not a double from Scala code.")
         case "Float" => if (typ != "num") throw new Exception("It was expected a '" + typ + "', not a float from Scala code.")
         case "Int" => if (typ != "num") throw new Exception("It was expected a '" + typ + "', not an int from Scala code.")
         case "Long" => if (typ != "num") throw new Exception("It was expected a '" + typ + "', not a long from Scala code.")
         case "Boolean" => if (typ != "boo") throw new Exception("It was expected a '" + typ + "', not a boolean from Scala code.")
         case "Char" => if (typ != "cha") throw new Exception("It was expected a '" + typ + "', not a char from Scala code.")
         case "Unit" => if (typ != "emp") throw new Exception("It was expected a '" + typ + "', not a () from Scala code.")
         case "List" => if (!typ.startsWith("[")) throw new Exception("It was expected a '" + typ + "', not a list from Scala code.")
         case x => throw new Exception(typ + " expected, but " + x + " coming from Scala code.")
      }
      interpret.valueOfTerm("resulting_value").get
   }

   def prt_as_list(l: L[Char]) {
      print("[" + l.l.mkString(",") + "]")
   }

   def prtln_as_list(l: L[Char]) {
      println("[" + l.l.mkString(",") + "]")
   }
}