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

   Do with it what you will. */

class TypeError(msg: String) extends Exception(msg)

class ParseError(msg: String) extends Exception(msg)

object TypeSystem {

   class Context(var env: Map[String, Type]) {
      def analyse(ast: Expr): Type = analyse(ast, Set.empty)

      def analyse(ast: Expr, nongen: Set[Variable]): Type = ast match {
         case AssignE(v, expr) =>
            val defntype = analyse(expr, nongen)
            env += v -> defntype
            analyse(expr, nongen)
         case ApplyE(fn, arg) =>
            val funtype = analyse(fn, nongen)
            val argtype = analyse(arg, nongen)
            val resulttype = newVariable
            unify(FunctionType(argtype, resulttype), funtype)
            resulttype
         case IdentE(name) =>
            gettype(name, nongen)
         case LambdaE(arg, body) =>
            val argtype = newVariable
            val newctx = new Context(env + (arg -> argtype))
            val resulttype = newctx.analyse(body, nongen + argtype)
            FunctionType(argtype, resulttype)
         case LetE(v, defn, body) =>
            val defntype = analyse(defn, nongen)
            val newctx = new Context(env + (v -> defntype))
            newctx.analyse(body, nongen)
         case LetrecE(v, defn, body) =>
            val newtype = newVariable
            val newctx = new Context(env + (v -> newtype))
            val defntype = newctx.analyse(defn, nongen + newtype)
            unify(newtype, defntype)
            newctx.analyse(body, nongen)
         case NumberE(_) =>
            NumberT
         case CharE(_) =>
            CharT
          case EmptyE => EmptyT
         case BlockE(l) =>
            val newctx = new Context(env)
            l.map {x => newctx.analyse(x, nongen)}.last

         //      case ListE(l)=>
      }

      def gettype(name: String, nongen: Set[Variable]): Type = {
         if (env.contains(name))
            fresh(env(name), nongen)
         else
            throw new ParseError("Undefined symbol " + name)
      }
   }

   abstract class Type

   case class Variable(id: Int) extends Type {
      var instance: Option[Type] = None
      lazy val name = nextUniqueName
   }

   case class TypeOperator(name: String, args: Seq[Type]) extends Type

   def FunctionType(from: Type, to: Type) = TypeOperator("→", Array(from, to))

   def ListType(elem_type: Type) = TypeOperator("[]", Array(elem_type))

   val NumberT = TypeOperator("'num'", Array[Type]())
   val CharT = TypeOperator("'cha'", Array[Type]())
   val BooleanT = TypeOperator("'boo'", Array[Type]())
   val EmptyT  = TypeOperator("'emp'", Array[Type]())
   var _nextVariableName = 'α'
   var _nextVariableId = 0

   def nextUniqueName = {
      val result = _nextVariableName
      _nextVariableName = (_nextVariableName.toInt + 1).toChar
      result.toString
   }

   def newVariable: Variable = {
      val result = _nextVariableId
      _nextVariableId += 1
      Variable(result)
   }

   def string(t: Type): String = t match {
      case v: Variable => v.instance match {
         case Some(i) => string(i)
         case None => v.name
      }
      case TypeOperator(name, args) => {
         if (args.length == 0)
            name
         else if (args.length == 1)
            "[" + string(args(0)) + "]"
         else if (args.length == 2)
            "(" + string(args(0)) + " " + name + " " + string(args(1)) + ")"
         else
            args.mkString(name + " ", " ", "")
      }
   }

   def fresh(t: Type, nongen: Set[Variable]) = {
      import scala.collection.mutable
      val mappings = new mutable.HashMap[Variable, Variable]
      def freshrec(tp: Type): Type = {
         prune(tp) match {
            case v: Variable =>
               if (isgeneric(v, nongen))
                  mappings.getOrElseUpdate(v, newVariable)
               else
                  v

            case TypeOperator(name, args) =>
               TypeOperator(name, args.map(freshrec(_)))
         }
      }
      freshrec(t)
   }

   def unify(t1: Type, t2: Type) {
      val type1 = prune(t1)
      val type2 = prune(t2)
      (type1, type2) match {
         case (a: Variable, b) => if (a != b) {
            if (occursintype(a, b))
               throw new TypeError("recursive unification")
            a.instance = Some(b)
         }
         case (a: TypeOperator, b: Variable) => unify(b, a)
         case (a: TypeOperator, b: TypeOperator) => {
            if (a.name != b.name ||
               a.args.length != b.args.length) throw new TypeError("Type mismatch: " + string(a) + "≠" + string(b))

            for (i <- 0 until a.args.length)
               unify(a.args(i), b.args(i))
         }
      }
   }

   // Returns the currently defining instance of t.
   // As a side effect, collapses the list of type instances.
   def prune(t: Type): Type = t match {
      case v: Variable if v.instance.isDefined => {
         val inst = prune(v.instance.get)
         v.instance = Some(inst)
         inst
      }
      case _ => t
   }

   // Note: must be called with v 'pre-pruned'
   def isgeneric(v: Variable, nongen: Set[Variable]) = !(occursin(v, nongen))

   // Note: must be called with v 'pre-pruned'
   def occursintype(v: Variable, type2: Type): Boolean = {
      prune(type2) match {
         case `v` => true
         case TypeOperator(name, args) => occursin(v, args)
         case _ => false
      }
   }

   def occursin(t: Variable, list: Iterable[Type]) =
      list exists (t2 => occursintype(t, t2))
}

object HindleyMilner {
   def main(args: Array[String]) {
      Console.setOut(new java.io.PrintStream(Console.out, true, "utf-8"))
      val var1 = TypeSystem.newVariable
      val var2 = TypeSystem.newVariable
      val pairtype = TypeSystem.TypeOperator("×", Array(var1, var2))
      val var3 = TypeSystem.newVariable

      val myenv = Map.empty ++ Array(
         "pair" -> TypeSystem.FunctionType(var1, TypeSystem.FunctionType(var2, pairtype)),
         "true" -> TypeSystem.BooleanT,
         "false" -> TypeSystem.BooleanT,
         "cond" -> TypeSystem.FunctionType(TypeSystem.BooleanT, TypeSystem.FunctionType(var3, TypeSystem.FunctionType(var3, var3))),
         "zero" -> TypeSystem.FunctionType(TypeSystem.NumberT, TypeSystem.BooleanT),
         "pred" -> TypeSystem.FunctionType(TypeSystem.NumberT, TypeSystem.NumberT),
         "times" -> TypeSystem.FunctionType(TypeSystem.NumberT, TypeSystem.FunctionType(TypeSystem.NumberT, TypeSystem.NumberT))
      )


      val pair = ApplyE(ApplyE(IdentE("pair"), ApplyE(IdentE("f"), NumberE("4"))), ApplyE(IdentE("f"), IdentE("true")))
      val examples = Array[Expr](
         // factorial
         LetrecE("factorial", // letrec factorial =
            LambdaE("n", // fn n =>
               ApplyE(
                  ApplyE(// cond (zero n) 1
                     ApplyE(IdentE("cond"), // cond (zero n)
                        ApplyE(IdentE("zero"), IdentE("n"))),
                     NumberE("1")),
                  ApplyE(// times n
                     ApplyE(IdentE("times"), IdentE("n")),
                     ApplyE(IdentE("factorial"),
                        ApplyE(IdentE("pred"), IdentE("n")))
                  )
               )
            ), // in
            ApplyE(IdentE("factorial"), NumberE("5"))
         ),

         //         Should fail:
         //         fn x => (pair(x(3) (x(true)))
         LambdaE("x",
            ApplyE(
               ApplyE(IdentE("pair"),
                  ApplyE(IdentE("x"), NumberE("3"))),
               ApplyE(IdentE("x"), IdentE("true")))),

         // pair(f(3), f(true))
         ApplyE(
            ApplyE(IdentE("pair"), ApplyE(IdentE("f"), NumberE("4"))),
            ApplyE(IdentE("f"), IdentE("true"))),


         // letrec f = (fn x => x) in ((pair (f 4)) (f true))
         LetE("f", LambdaE("x", IdentE("x")), pair),

         // fn f => f f (fail)
         LambdaE("f", ApplyE(IdentE("f"), IdentE("f"))),

         // let g = fn f => 5 in g g
         LetE("g",
            LambdaE("f", NumberE("5")),
            ApplyE(IdentE("g"), IdentE("g"))),

         // example that demonstrates generic and non-generic variables:
         // fn g => let f = fn x => g in pair (f 3, f true)
         LambdaE("g",
            LetE("f",
               LambdaE("x", IdentE("g")),
               ApplyE(
                  ApplyE(IdentE("pair"),
                     ApplyE(IdentE("f"), NumberE("3"))
                  ),
                  ApplyE(IdentE("f"), IdentE("true"))))),

         // FunctionType composition
         // fn f (fn g (fn arg (f g arg)))
         LambdaE("f", LambdaE("g", LambdaE("arg", ApplyE(IdentE("g"), ApplyE(IdentE("f"), IdentE("arg")))))),

         AssignE("f", LambdaE("x", IdentE("x")))
      )
      for (eg <- examples) {
         tryexp(new Context(myenv), eg)
      }
   }

   def tryexp(ctx:Context, ast: Expr) {
      print(Expr.string(ast) + " : ")
      try {
         val t = ctx.analyse(ast)
         print(TypeSystem.string(t))

      } catch {
         case t: ParseError => print(t.getMessage)
         case t: TypeError => print(t.getMessage)
      }
      println()
   }
}
