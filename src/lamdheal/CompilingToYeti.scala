package lamdheal

import lamdheal.TypeSystem._
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

object CompilingToYeti {

   //   def scalaType(typ: Type): String = typ match {
   //      case FunctionT(from, to) => "((" + scalaType(from) + ") => " + scalaType(to) + ")"
   //      case ListT(eT) => "L" + "[" + scalaType(eT) + "]"
   //      case NumberT => "Double"
   //      case BooleanT => "Boolean"
   //      case CharT => "Char"
   //      case EmptyT => "Unit"
   //      case VariableT(_) => "Any"
   //   }

   def run(ex: Expr): String = {
      ex match {
         case ApplyE(f, a) => if (f.t != null && f.t.getClass == classOf[ListT]) "( map " + run(a) + " " + run(f) + ")" else "(" + run(f) + " " + run(a) + ")"
         //         case ApplyE(f, a) => run(f) + "(" + run(a) + ")"
         case AssignE(id, e) => "" + id + "=" + run(e) + ""
         case BlockE(l) => "(\n" + l.filterNot(EmptyE ==).map(x => "   " + run(x)).mkString(";\n") + ";\n)\n"
         case BooleanE(b) => b
         case CharE(c) => "'" + c + "'"
         case EmptyE => "()"
         case IdentE(id) => id match {
            case "!" => "reverse"
            case "@" => "head"
            case "~" => "tail"
            case ".." => "do x y: [x .. y] done"
            case BuiltinId.println => "println"
            case BuiltinId.print => "print"
            case BuiltinId.printastext => "(print . fold (^) \"\")"
            case BuiltinId.printlnastext => "(println . fold (^) \"\")"
            case "(\\)" => "do x y: round(x/y) done"
            case x => x
         }
         case lambda@LambdaE(arg, body) => "do " + arg + ":" + " " + run(body) + " done"
         case liste@ListE(l) => "[" + l.map(run).mkString(",") + "]"
         case NumberE(n) => n
         case TypeE(t) => throw new Exception("run_java not implemented yet!") //\"" + t + "\""
      }
   }

   def compile_and_run(expr: Expr) {
      val source = "program output;\n" +
         "print " + run(expr) + " "
      YetiCompiler.external_run(source)
   }
}
