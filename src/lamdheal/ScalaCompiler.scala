package lamdheal


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

object ScalaCompiler {
   def external_run(s: String) {
      import java.io.FileWriter
      val fw = new FileWriter("output.scala")
      fw.write(s)
      fw.close()
      import sys.process.Process
      val gera = Process("scala output.scala")
      try {
         gera.!!
      } catch {
         case _: Throwable => println(s)
      } finally {
         gera.lines map println
      }
   }

   def compile(str: String) {
      import scala.reflect.runtime._
      val cm = universe.runtimeMirror(getClass.getClassLoader)
      import scala.tools.reflect.ToolBox
      val tb = cm.mkToolBox()
      tb.eval(tb.parse(str))
   }

   def interpret(str: String) {
      import java.io.{FileOutputStream, PrintStream}
      val out = new PrintStream(new FileOutputStream("/dev/null"))
      val flusher = new java.io.PrintWriter(out) // System.out
      val interpret = {
         val settings = new scala.tools.nsc.GenericRunnerSettings(println)
         settings.usejavacp.value = true
         //         settings.classpath.value="/home/davi/unversioned/software/scala-2.10.2/lib/scala-reflect.jar:/home/davi/unversioned/software/scala-2.10.2/lib/scala-compiler
         // .jar:/home/davi/unversioned/software/scala-2.10.2/lib/scala-library.jar"
         new scala.tools.nsc.interpreter.IMain(settings, flusher)
      }

      interpret.interpret(str)
   }

   def main(args: Array[String]) {
      val i = System.currentTimeMillis()
      val script = "val ii=System.currentTimeMillis();" +
         "   var n = 1d\n   var n_1 = 0d\n   var fibo = 0d\n   var j=0d\n   var i=1d\n   var res=0d\n" +
         "   while(i<=19977) {\t      \n      n=1\n      n_1=0\n      j=1\n      while(j<=19977) {\t      \n         fibo = (i + " +
         "n + n_1 + j) / 123456d\n         n_1 = n\n         n = fibo\n         j+=1\n      }\n      res += fibo\n      i+=1\n   }\n" +
         "   println(res)\n" +
         "println((System.currentTimeMillis() - ii)/1000.0 + \"\\n\")"
      compile(script)
      println((System.currentTimeMillis() - i) / 1000.0 + " <-\n")
   }
}
