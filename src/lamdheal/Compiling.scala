package lamdheal

import java.io.StringReader
import org.codehaus.janino.SimpleCompiler

//import org.codehaus.commons.compiler.jdk.SimpleCompiler

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

   import TypeSystem._

   def return_type(t: Type): String = t match {
      case NumberT => "double"
      case EmptyT => "Object"
      case v@VariableT(i) => return_type(v.instance.get) //case None ??
   }

   def run(ex: Expr): String = {
        ex match {
         case EmptyE => ""
         //         case la@LambdaE(param, BlockE(l)) =>
         //            val from_type = la.t.asInstanceOf[FunctionT].from match {
         //               case NumberT => "double"
         //               case EmptyT => "void"
         //            }
         //            val to_type = la.t.asInstanceOf[FunctionT].to match {
         //               case NumberT => "double"
         //               case EmptyT => "void"
         //            }
         //            "new Anon() { public " + to_type + " f(" + from_type + " " + param + ") {\n" +
         //               l.filterNot(EmptyExpr ==).map(run).map("   " +).mkString(";\n") +
         //               "};\n"

         case b@BlockE(l) =>
            val statements = l.filterNot(EmptyE ==).map(run).map("   " +)
            if (statements.length > 0) {
               "new Anon() { public " + return_type(b.t) + " f() {\n" +
                  statements.dropRight(1).mkString(";\n") + ";\n" +
                  "return " + (if (return_type(b.t) != "void") statements.last + ";" else " new Empty();") + "\n" +
                  "} }.f()\n"
            } else "new Empty()"
         //
         //         case Assign(id, expr) =>
         //            var_counter += 1
         //            val tranlated_id = id + var_counter.toString
         //            translation += (id -> tranlated_id)
         //            //               println(expr + " before match")
         //            expr.t match {
         //               case NumberT => "final double[] " + tranlated_id + " = {" + run(expr) + "};\n"
         //               case ListT(CharacterT) => "final String " + tranlated_id + " = " + run(expr) + ";\n"
         //               case ListT(_) => "final ArrayList " + tranlated_id + " = " + run(expr) + ";\n"
         //            }
         case c: CharE => "'" + c.toString + "'"
         case ApplyE(f, a) => (f, a) match {
            //                     case (ShowE, x) => x + ".toString()"
            case (TypeE(t), ListE(l)) =>
               val java_lines = l.map(_.asInstanceOf[CharE].c).mkString.split("\n")
               val code = if (t == EmptyT) {
                  java_lines.mkString(";\n") + "return new Empty();"
               } else {
                  java_lines.dropRight(1).mkString(";\n") + "return " + java_lines.last + ";\n"
               }
               "new Anon() { public " + return_type(t) + " f() {\n" +
                  code + "\n" +
                  "} }.f()\n"

            case (y, x) => run(y) + "(" + run(x) + ")"

         }
         //         case PrintLnE => "System.out.println"
         //         case Ident(name) => translation(name)
         //         case NumberExpr(n) => n.toString
         case ListE(l) => "Arrays.asList(" + l.map(run).mkString(", ") + ")"
         //         case ListInterval(i, f) => "range(" + run(i) + "," + run(f) + ")"
      }
   }

   def compile(expr: Expr) {
      val source = "import java.util.ArrayList;\n" +
         "import java.util.Iterator;\n" +
         "class Empty {\n" +
         "   String toString() {" +
         "      return \"Ø\";\n" +
         "   }" +
         "}\n" +
         "interface Anon {\n" +
         "}\n" +
         "public class RuntimeMain implements Runnable {\n" +
         "   public static void main(String[] args) {\n" +
         "      RuntimeMain m = new RuntimeMain();\n" +
         "      m.run();\n" +
         "   }\n" +
         "    \n" +
         "   public ArrayList range(Double i, Double f) {\n" +
         "      ArrayList al = new ArrayList();\n" +
         "      for (Double x=i; x<=f; x++)\n" +
         "         al.add(x);\n" +
         "      return al;\n" +
         "   }\n" +
         "    \n" +
         "   public void run() {\n" +
         "      System.out.println(\n" +
         run(expr).split('\n').map("         " +).mkString("\n") + "\n" +
         "      );\n" +
         "   }\n" +
         "}\n"

      println(source)

      val compiler = new SimpleCompiler()
      compiler.cook(new StringReader(source))
      val clss = compiler.getClassLoader.loadClass("RuntimeMain")
      val eval = clss.newInstance().asInstanceOf[Runnable]
      eval.run()
   }

}
