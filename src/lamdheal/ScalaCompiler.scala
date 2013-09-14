package lamdheal

/*
 * Copyright 2010 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//import scala.tools.nsc.{Global, Settings}
//import scala.reflect.internal.util.BatchSourceFile
//import tools.nsc.io.{VirtualDirectory, AbstractFile}
//import tools.nsc.interpreter.AbstractFileClassLoader
//import java.security.MessageDigest
//import java.math.BigInteger
//import collection.mutable
//import java.io.File
//
//object CompileTest {
//      val compiler = new ScalaCompiler(Some(new File("/tmp/d")))
////   val compiler = new ScalaCompiler(None)
//
//   def main(args: Array[String]) {
//val script = "val ii=System.currentTimeMillis();" +
//   "   var n = 1d\n   var n_1 = 0d\n   var fibo = 0d\n   var j=0d\n   var i=1d\n   var res=0d\n" +
//   "   while(i<=19977) {\t      \n      n=1\n      n_1=0\n      j=1\n      while(j<=19977) {\t      \n         fibo = (i + " +
//   "n + n_1 + j) / 123456d\n         n_1 = n\n         n = fibo\n         j+=1\n      }\n      res += fibo\n      i+=1\n   }\n" +
//   "   println(res)\n" +
//   "println((System.currentTimeMillis() - ii)/1000.0 + \"\\n\")"

//      val i = System.currentTimeMillis()
//      compiler.eval[Unit](script)
//      println((System.currentTimeMillis() - i) / 1000.0 + "\n")
//   }
//}
//
//class ScalaCompiler(targetDir: Option[File]) {
//
//   val target = targetDir match {
//      case Some(dir) => AbstractFile.getDirectory(dir)
//      case None => new VirtualDirectory("(memory)", None)
//   }
//
//   val classCache = mutable.Map[String, Class[_]]()
//
//   private val settings = new Settings()
//   settings.deprecation.value = false // enable detailed deprecation warnings
//   settings.unchecked.value = false // enable detailed unchecked warnings
//   settings.outputDirs.setSingleOutput(target)
//   settings.usejavacp.value = true
//   settings.nobootcp.value=true
//
//   private val global = new Global(settings)
//   private lazy val run = new global.Run
//
//   val classLoader = new AbstractFileClassLoader(target, this.getClass.getClassLoader)
//
//   /** Compiles the code as a class into the class loader of this compiler.
//     *
//     * @param code
//     * @return
//     */
//   def compile(code: String) = {
//      val className = "Lamdheal" //classNameForCode(code)
//      findClass(className).getOrElse {
//         val sourceFiles = List(new BatchSourceFile("(inline)", wrapCodeInClass(className, code)))
//         run.compileSources(sourceFiles)
//         findClass(className).get
//      }
//   }
//
//   /** Compiles the source string into the class loader and
//     * evaluates it.
//     *
//     * @param code
//     * @tparam T
//     * @return
//     */
//   def eval[T](code: String): T = {
//      val cls = compile(code)
//      cls.getConstructor().newInstance().asInstanceOf[() => Any].apply().asInstanceOf[T]
//   }
//
//   def findClass(className: String): Option[Class[_]] = {
//      synchronized {
//         classCache.get(className).orElse {
//            try {
//               val cls = classLoader.loadClass(className)
//               classCache(className) = cls
//               Some(cls)
//            } catch {
//               case e: ClassNotFoundException => None
//            }
//         }
//      }
//   }
//
//   protected def classNameForCode(code: String): String = {
//      val digest = MessageDigest.getInstance("SHA-1").digest(code.getBytes)
//      "sha" + new BigInteger(1, digest).toString(16)
//   }
//
//   /*
//   * Wrap source code in a new class with an apply method.
//   */
//   private def wrapCodeInClass(className: String, code: String) = {
//      "class " + className + " extends (() => Any) {\n" +
//         "  def apply() = {\n" +
//         code + "\n" +
//         "  }\n" +
//         "}\n"
//   }
//}

object ScalaCompiler {
   def main(args: Array[String]) {
      val i = System.currentTimeMillis()
      val script = "val ii=System.currentTimeMillis();" +
         "   var n = 1d\n   var n_1 = 0d\n   var fibo = 0d\n   var j=0d\n   var i=1d\n   var res=0d\n" +
         "   while(i<=19977) {\t      \n      n=1\n      n_1=0\n      j=1\n      while(j<=19977) {\t      \n         fibo = (i + " +
         "n + n_1 + j) / 123456d\n         n_1 = n\n         n = fibo\n         j+=1\n      }\n      res += fibo\n      i+=1\n   }\n" +
         "   println(res)\n" +
         "println((System.currentTimeMillis() - ii)/1000.0 + \"\\n\")"
      import scala.reflect.runtime._
      val cm = universe.runtimeMirror(getClass.getClassLoader)
      import scala.tools.reflect.ToolBox
      val tb = cm.mkToolBox()
      tb.eval(tb.parse(script))
      println((System.currentTimeMillis() - i) / 1000.0 + " <-\n")
   }
}
