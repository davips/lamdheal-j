package lamdheal

import io.Source
import org.codehaus.janino.ClassBodyEvaluator
import java.util
import lamdheal.TypeSystem.{Context, Type}

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

object Main extends App {
   Console.setOut(new java.io.PrintStream(Console.out, true, "utf-8"))
   val myenv = Map() ++ Array(
      "true" -> TypeSystem.BooleanT,
      "false" -> TypeSystem.BooleanT
   )
   if (args.length != 1) {
      println("Usage:\njava -jar lamdheal.jar input-file.java")
      val source_code = Source.fromFile("example.java").getLines().mkString("\n")
      val ast = LamdhealParser.parse(source_code)
      val typed_ast = HindleyMilner.tryexp(new Context(myenv), ast)
   }
   else {
      val source_code = Source.fromFile(args(0)).getLines().mkString("\n")
   }
}
