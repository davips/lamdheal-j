package lamdheal

import lamdheal.TypeSystem.Context

/* Copyright 2013 Davi Pereira dos Santos (only regarding most recent changes)
   This file is part of Lamdheal (only regarding most recent changes)

   Based on Andrew Forrest's 2009 adaptation for Scala
   http://dysphoria.net/code/hindley-milner/HindleyMilner.scala

   Implementation of basic polymorphic type-checking for a simple language.
   Based heavily on Nikita Borisov’s Perl implementation at
   http://web.archive.org/web/20050420002559/www.cs.berkeley.edu/~nikitab/courses/cs263/hm.html
   which in turn is based on the paper by Luca Cardelli at
   http://lucacardelli.name/Papers/BasicTypechecking.pdf

   If you run it with "scala HindleyMilner.scala" it will attempt to report the types
   for a few example expressions. (It uses UTF-8 for output, so you may need to set your
   terminal accordingly.)

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

class TypeError(msg: String) extends Exception(msg)

class ParseError(msg: String) extends Exception(msg)

object TypeSystem {

   abstract class Type {
      //      val name: String
   }

   case class VariableT(id: Int) extends Type {
      var instance: Option[Type] = None
      lazy val name = nextUniqueName
   }

   //   case class TypeOperator(name: String, args: Seq[Type]) extends Type

   case class FunctionT(from: Type, to: Type) extends Type {
      override def toString = "→"
   }

   case class ListT(elem_type: Type) extends Type {
   }

   case object CharT extends Type {
   }

   case object NumberT extends Type {
   }

   case object BooleanT extends Type {
   }

   case object EmptyT extends Type {
   }

   //TypeOperator("[" + elem_type + "]", Array(elem_type))

   var _nextVariableName = 'α'
   var _nextVariableId = 0
   val default_env = Map() ++ Array(
      "true" -> BooleanT,
      "false" -> BooleanT,

      "(+)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(-)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(*)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(/)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(\\)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(%)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(^)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),

      "(==)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(!=)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(>=)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(<=)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(>)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT)),
      "(<)" -> FunctionT(NumberT, FunctionT(NumberT, NumberT))
   )

   class Context(var env: Map[String, Type]) {
      def analyse(ast: Expr): Type = analyse(ast, Set.empty)

      def analyse(ast: Expr, nongen: Set[VariableT]): Type = ast match {
         case AssignE(v, expr) =>
            val defntype = analyse(expr, nongen)
            env += v -> defntype
            analyse(expr, nongen)
         case ApplyE(fn, arg) =>
            val funtype = analyse(fn, nongen)
            val argtype = analyse(arg, nongen)
            val resulttype = newVariable
            try {
               unify(FunctionT(argtype, resulttype), funtype)
            } catch {
               case e: TypeError => throw new TypeError("at line " + ": " + e.getMessage +
                  "\nHint:\n" + FunctionT(argtype, resulttype) + "\ndiffers from\n" + funtype + "\n.")
            }
            resulttype
         case CharE(_) =>
            CharT
         case b@BlockE(l) =>
            if (l.length > 0) {
               val newctx = new Context(env)
               b.t = l.map {
                  case x =>
                     val xt = newctx.analyse(x, nongen)
                     print(x + ": " + xt + ".   ")
                     xt
               }.last
               b.t
            } else {
               b.t = EmptyT
               b.t
            }
         case EmptyE => EmptyT
         case IdentE(name) =>
            gettype(name, nongen)
         case LambdaE(arg, body) =>
            val argtype = newVariable
            val newctx = new Context(env + (arg -> argtype))
            val resulttype = newctx.analyse(body, nongen + argtype)
            FunctionT(argtype, resulttype)
         //         case LetE(v, defn, body) =>
         //            val defntype = analyse(defn, nongen)
         //            val newctx = new Context(env + (v -> defntype))
         //            newctx.analyse(body, nongen)
         //         case LetrecE(v, defn, body) =>
         //            val newtype = newVariable
         //            val newctx = new Context(env + (v -> newtype))
         //            val defntype = newctx.analyse(defn, nongen + newtype)
         //            unify(newtype, defntype)
         //            newctx.analyse(body, nongen)
         case li@ListE(exs) =>
            if (exs.length > 0) {
               val element_types = exs map (e => analyse(e, nongen))
               if (li.t == null) li.t = ListT(element_types.head)
               try {
                  element_types.tail map (unify(element_types.head, _))
               } catch {
                  case e: TypeError => throw new TypeError("at line " + ": " + e.getMessage +
                     "\nHint: all elements should have the same type inside a list." +
                     "\nHint: it seems like at least one element in " + li.t + " is not of type " + element_types.head + ".")
               }
            }
            li.t
         case NumberE(_) =>
            NumberT
         case TypeE(t) =>
            FunctionT(ListT(CharT), t)
      }

      def gettype(name: String, nongen: Set[VariableT]): Type = {
         if (env.contains(name))
            fresh(env(name), nongen)
         else
            throw new ParseError("Undefined symbol " + name)
      }
   }

   def nextUniqueName = {
      val result = _nextVariableName
      _nextVariableName = (_nextVariableName.toInt + 1).toChar
      result.toString
   }

   def newVariable: VariableT = {
      val result = _nextVariableId
      _nextVariableId += 1
      VariableT(result)
   }

   //   def string(t: Type): String = t match {
   //      case v: VariableT => v.instance match {
   //         case Some(i) => string(i)
   //         case None => v.name
   //      }
   //      case FunctionT(from,to) =>
   //         string(from) + "→" + string(to)
   //      case ListT(arg) =>
   //         "[" + string(arg) + "]"
   //      case CharT => "cha"
   //      case NumberT => "num"
   //   }

   def fresh(t: Type, nongen: Set[VariableT]) = {
      import scala.collection.mutable
      val mappings = new mutable.HashMap[VariableT, VariableT]
      def freshrec(tp: Type): Type = {
         prune(tp) match {
            case v: VariableT =>
               if (isgeneric(v, nongen))
                  mappings.getOrElseUpdate(v, newVariable)
               else
                  v

            //            case TypeOperator(name, args) =>
            //               TypeOperator(name, args.map(freshrec(_)))
            case FunctionT(from, to) =>
               FunctionT(freshrec(from), freshrec(to))
            case ListT(arg) =>
               ListT(freshrec(arg))
            case CharT => CharT
            case NumberT => NumberT
         }
      }
      freshrec(t)
   }

   def unify(t1: Type, t2: Type) {
      val type1 = prune(t1)
      val type2 = prune(t2)
      (type1, type2) match {
         case (a: VariableT, b) => if (a != b) {
            if (occursintype(a, b))
               throw new TypeError("recursive unification")
            a.instance = Some(b)
         }
         case (a, b: VariableT) => unify(b, a) //         case (a: TypeOperator, b: VariableT) => unify(b, a)
         //         case (a: TypeOperator, b: TypeOperator) => {
         //            if (a.name != b.name ||
         //               a.args.length != b.args.length) throw new TypeError("Type mismatch: " + string(a) + "≠" + string(b))
         //
         //            for (i <- 0 until a.args.length)
         //               unify(a.args(i), b.args(i))
         //         }
         case (a: ListT, b: ListT) => {
            //            if (a.name != b.name) throw new TypeError("Type mismatch: " + string(a) + "≠" + string(b))
            unify(a.elem_type, b.elem_type)
         }
         case (a: FunctionT, b: FunctionT) => {
            //            if (a.name != b.name) throw new TypeError("Type mismatch: " + string(a) + "≠" + string(b))
            unify(a.from, b.from)
            unify(a.to, b.to)
         }
         case (CharT, CharT) | (NumberT, NumberT) => //Ok.
         case (a, b) => throw new TypeError("Type mismatch: " + a + "≠" + b)
      }
   }

   // Returns the currently defining instance of t.
   // As a side effect, collapses the list of type instances.
   def prune(t: Type): Type = t match {
      case v: VariableT if v.instance.isDefined => {
         val inst = prune(v.instance.get)
         v.instance = Some(inst)
         inst
      }
      case _ => t
   }

   // Note: must be called with v 'pre-pruned'
   def isgeneric(v: VariableT, nongen: Set[VariableT]) = !(occursin(v, nongen))

   // Note: must be called with v 'pre-pruned'
   def occursintype(v: VariableT, type2: Type): Boolean = {
      prune(type2) match {
         case `v` => true
         //         case TypeOperator(name, args) => occursin(v, args)
         case ListT(arg) => occursin(v, Array(arg)) //adicionei
         case FunctionT(from, to) => occursin(v, Array(from, to)) //adicionei
         case _ => false
      }
   }

   def occursin(t: VariableT, list: Iterable[Type]) =
      list exists (t2 => occursintype(t, t2))
}

object HindleyMilner {
   def main(args: Array[String]) {
      Console.setOut(new java.io.PrintStream(Console.out, true, "utf-8"))
      //      val var1 = TypeSystem.newVariable
      //      val var2 = TypeSystem.newVariable
      //      val pairtype = TypeSystem.TypeOperator("×", Array(var1, var2))
      //      val var3 = TypeSystem.newVariable

      val myenv = Map.empty ++ Array(
         //         "pair" -> TypeSystem.FunctionT(var1, TypeSystem.FunctionT(var2, pairtype)),
         "true" -> TypeSystem.BooleanT,
         "false" -> TypeSystem.BooleanT,
         //         "cond" -> TypeSystem.FunctionT(TypeSystem.BooleanT, TypeSystem.FunctionT(var3, TypeSystem.FunctionT(var3, var3))),
         "zero" -> TypeSystem.FunctionT(TypeSystem.NumberT, TypeSystem.BooleanT),
         "pred" -> TypeSystem.FunctionT(TypeSystem.NumberT, TypeSystem.NumberT),
         "times" -> TypeSystem.FunctionT(TypeSystem.NumberT, TypeSystem.FunctionT(TypeSystem.NumberT, TypeSystem.NumberT))
      )


      val pair = ApplyE(ApplyE(IdentE("pair"), ApplyE(IdentE("f"), NumberE("4"))), ApplyE(IdentE("f"), IdentE("true")))
      val examples = Array[Expr](
         // factorial
         //         LetrecE("factorial", // letrec factorial =
         //            LambdaE("n", // fn n =>
         //               ApplyE(
         //                  ApplyE(// cond (zero n) 1
         //                     ApplyE(IdentE("cond"), // cond (zero n)
         //                        ApplyE(IdentE("zero"), IdentE("n"))),
         //                     NumberE("1")),
         //                  ApplyE(// times n
         //                     ApplyE(IdentE("times"), IdentE("n")),
         //                     ApplyE(IdentE("factorial"),
         //                        ApplyE(IdentE("pred"), IdentE("n")))
         //                  )
         //               )
         //            ), // in
         //            ApplyE(IdentE("factorial"), NumberE("5"))
         //         ),

         //         Should fail:
         //         fn x => (pair(x(3) (x(true)))
         LambdaE("x",
            ApplyE(
               ApplyE(IdentE("pair"),
                  ApplyE(IdentE("x"), NumberE("3"))),
               ApplyE(IdentE("x"), IdentE("true")))),


         // letrec f = (fn x => x) in ((pair (f 4)) (f true))
         AssignE("f", LambdaE("x", IdentE("x"))),
         pair,

         // fn f => f f (fail)
         LambdaE("f", ApplyE(IdentE("f"), IdentE("f"))),

         // let g = fn f => 5 in g g
         AssignE("g",
            LambdaE("f", NumberE("5"))),
         ApplyE(IdentE("g"), IdentE("g")),

         // example that demonstrates generic and non-generic variables:
         // fn g => let f = fn x => g in pair (f 3, f true)
         LambdaE("g",
            BlockE(Array(AssignE("f",
               LambdaE("x", IdentE("g"))),
               ApplyE(
                  ApplyE(IdentE("pair"),
                     ApplyE(IdentE("f"), NumberE("3"))
                  ),
                  ApplyE(IdentE("f"), IdentE("true")))))),

         // FunctionT composition
         // fn f (fn g (fn arg (f g arg)))
         LambdaE("f", LambdaE("g", LambdaE("arg", ApplyE(IdentE("g"), ApplyE(IdentE("f"), IdentE("arg")))))),

         AssignE("f", LambdaE("x", IdentE("x")))
      )
      val nc = new Context(myenv)
      for (eg <- examples) {
         verify(eg, nc)
      }
   }


   def verify(ast: Expr, ctx: Context = new Context(TypeSystem.default_env)) {
      print(ast + ": ")
      try {
         val t = ctx.analyse(ast)
         print(t + ".   ")

      } catch {
         case t: ParseError => print(t.getMessage)
         case t: TypeError => print(t.getMessage)
      }
      println()
   }
}
