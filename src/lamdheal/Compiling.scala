package lamdheal

import lamdheal.TypeSystem._

/*  Copyright 2013 Davi Pereira dos Santos
    This file is part of Lamdheal.

    Lamdheal is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Lamdheal is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Lamdheal.  If not, see <http://www.gnu.org/licenses/>.*/

object Compiling {
   var with_runtime = false

   def scalaType(typ: Type): String = typ match {
      case ListT(eT) => "List[" + scalaType(eT) + "]"
      case NumberT => "Double"
      case BooleanT => "Boolean"
      case CharT => "Char"
      case EmptyT => "Unit"
   }

   def run(ex: Expr): String = {
      ex match {
         case ApplyE(f, a) => if (f.t != null && f.t.toString.startsWith("[")) run(f) + ".map(" + run(a) + ")" else run(f) + "(" + run(a) + ")"
         case AssignE(id, e) => "val " + id + "=" + run(e)
         case BlockE(l) => "{" + l.map(run).mkString("\n") + "}"
         case BooleanE(b) => b
         case CharE(c) => "'" + c + "'"
         case EmptyE => "Unit"
         case IdentE(id) => id match {
            case "`" => "println"
            case "`+" => "print"
            case "(*)" => "((x:Double) => (y:Double) => x*y)"
            case "(/)" => "((x:Double) => (y:Double) => x/y)"
            case "(\\)" => "((x:Double) => (y:Double) => math.round(x/y))"
            case "(%)" => "((x:Double) => (y:Double) => x%y)"
            case "(+)" => "((x:Double) => (y:Double) => x+y)"
            case "(-)" => "((x:Double) => (y:Double) => x-y)"
            case "(>=)" => "((x:Double) => (y:Double) => x>=y)"
            case "(<=)" => "((x:Double) => (y:Double) => x<=y)"
            case "(>)" => "((x:Double) => (y:Double) => x>y)"
            case "(<)" => "((x:Double) => (y:Double) => x<y)"
            case "(==)" => "((x:Any) => (y:Any) => x==y)"
            case "(!=)" => "((x:Any) => (y:Any) => x!=y)"
            case x => x
         }
         case lambda@LambdaE(arg, body) => "(" + arg + ":" + scalaType(lambda.t.asInstanceOf[FunctionT].from) + ") => {" + run(body) + "}"
         case ListE(l) => "List(" + l.map(run).mkString(",") + ")"
         case NumberE(n) => n
         case TypeE(t) => {with_runtime = true; "Runtime.interpret(\"" + t + "\")"}
      }
   }

   def compile(expr: Expr) {
      val i = System.currentTimeMillis()
      val source_core = run(expr)
      val source = if (with_runtime)
         "object Runtime {\n   def interpret(typ: String)(lst: List[Char]) = {\n      val str = \"val " +
            "resulting_value = \" + lst.mkString\n      import java.io.{FileOutputStream, PrintStream}\n      val out = new PrintStream(new FileOutputStream(\"/dev/null\"))\n      val flusher = new java.io.PrintWriter" +
            "(out)\n      val interpret = {\n         val settings = new scala.tools.nsc.GenericRunnerSettings(println)\n         settings.usejavacp.value = true\n         new scala.tools.nsc.interpreter.IMain" +
            "(settings, flusher)\n      }\n      interpret.interpret(str)\n      val resulting_type = interpret.typeOfTerm(\"resulting_value\")\n      resulting_type.toString() match {\n         case \"String\" => if " +
            "(typ != \"[cha]\") throw new Exception(\"It was expected a '\" + typ + \"', not a string from Scala code.\")\n         case \"Double\" => if (typ != \"num\") throw new Exception(\"It was expected a '\" + " +
            "typ + \"', not a double from Scala code.\")\n         case \"Float\" => if (typ != \"num\") throw new Exception(\"It was expected a '\" + typ + \"', " +
            "not a float from Scala code.\")\n         case \"Int\" => if (typ != \"num\") throw new Exception(\"It was expected a '\" + typ + \"', not an int from Scala code.\")\n         case \"Long\" => if (typ != " +
            "\"num\") throw new Exception(\"It was expected a '\" + typ + \"', not a long from Scala code.\")\n         case \"Boolean\" => if (typ != \"boo\") throw new Exception(\"It was expected a '\" + typ + \"', " +
            "not a boolean from Scala code.\")\n         case \"Char\" => if (typ != \"cha\") throw new Exception(\"It was expected a '\" + typ + \"', not a char from Scala code.\")\n         case \"Unit\" => if (typ " +
            "!= \"emp\") throw new Exception(\"It was expected a '\" + typ + \"', not a () from Scala code.\")\n         case \"List\" => if (!typ.startsWith(\"[\")) throw new Exception(\"It was expected a '\" + typ +" +
            " \"', not a list from Scala code.\")\n         case x => throw new Exception(typ + \" expected, but \" + x + \" coming from Scala code.\")\n      }\n      interpret.valueOfTerm(\"resulting_value\").get\n " +
            "  }\n}" +
            "\n" + source_core + "\n"
      else source_core
      //            println(source)
      //      ScalaCompiler.compile(source)
      //      ScalaCompiler.interpret(source)
      ScalaCompiler.external_run(source)
      println((System.currentTimeMillis() - i) / 1000.0 + " <-\n")
   }

   //   def executa_shell(code: String) = {
   //      val gera = Process(code)
   //      gera.!! //run()
   //      ListExpr(gera.lines.toArray.map(x => ListExpr(x.toCharArray map CharacterExpr)))
   //   }
}
