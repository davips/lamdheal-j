package lamdheal

import lamdheal.TypeSystem._
import java.io.FileReader
import scala.io.Source

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

object CompilingToScala {
   var with_runtime = false

   def scalaType(typ: Type): String = typ match {
      case FunctionT(from, to) => "((" + scalaType(from) + ") => " + scalaType(to) + ")"
      case ListT(eT) => "L" + "[" + scalaType(eT) + "]"
      case NumberT => "Double"
      case BooleanT => "Boolean"
      case CharT => "Char"
      case EmptyT => "Unit"
      case VariableT(_) => "Any"
   }

   def run(ex: Expr): String = {
      ex match {
         //         case ApplyE(f, a) => if (f.t != null && f.t.toString.startsWith("[")) run(f) + ".map(" + run(a) + ")" else run(f) + "(" + run(a) + ")"
         case ApplyE(f, a) => run(f) + "(" + run(a) + ")"
         case AssignE(id, e) => "val " + id + "=" + run(e)
         case BlockE(l) => "{" + l.map(run).mkString("\n") + "}" //Block: { } ou ( ) ?
         case BooleanE(b) => b
         case CharE(c) => "'" + c + "'"
         case EmptyE => "Unit"
         case IdentE(id) => id match {
            case "`" => "println"
            case "`+" => "print"
            case "`]" => "prtln_as_list"
            case "`]+" => "prt_as_list"
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
         case liste@ListE(l) =>
            val eT = liste.t.asInstanceOf[ListT].elem_type
            " new " + scalaType(liste.t) + "(List(" + l.map(run).mkString(",") + "))"
         case NumberE(n) => n
         case TypeE(t) => {with_runtime = true; "interpret(\"" + t + "\")"}
      }
   }

   def compile_and_run(expr: Expr) {
      val source = Source.fromFile("Runtime.scala").toList.dropRight(1).mkString + "\n" +
         run(expr) + "\n" +
         "}\n//Runtime.main(Array())"
//         "//AntiBug.main(Array())"
      //      println(source)
//            ScalaCompiler.interpret(source)
      ScalaCompiler.external_run(source)
   }

   //   def executa_shell(code: String) = {
   //      val gera = Process(code)
   //      gera.!! //run()
   //      ListExpr(gera.lines.toArray.map(x => ListExpr(x.toCharArray map CharacterExpr)))
   //   }
}
