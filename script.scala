/**
 * Not to use.
 * This a copy of runtime def.
 */
implicit class IntWithTimes[T](x: List[T]) {
   def apply[A](f: (T) => A): Unit = {
      x map f
   }
}
object Runtime {
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
}
{Unit
   Unit
   val a=List(1,2,3,Runtime.interpret("num")(List('S','y','s','t','e','m','.','c','u','r','r','e','n','t','T','i','m','e','M','i','l','l','i','s','(',')')))
   Unit
   Unit
   println(a)}