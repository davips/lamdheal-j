package lamdheal

import util.parsing.combinator.{JavaTokenParsers, ImplicitConversions, RegexParsers}
import lamdheal.TypeSystem.TypeOperator

/*  Copyright 2013 Davi Pereira dos Santos
    This file is part of Lamdheal.
    Guidelines from Masterarbeit of Eugen Labun were very helpful to begin writing the parser.

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
object LamdhealParser extends RegexParsers with ImplicitConversions with JavaTokenParsers {
   /**
    * Converts a text of code into an AST.
    * @param text source code
    * @return AST
    */
   def parse(text: String): Expr = {
      val u = parseAll(program, text)
      u match {
         case Success(t, next) => {
            u.get
         }
         case f => {
            throw new Exception("" + f)
         }
      }
   }

   def program = """/\*""".r ~> block <~ """\*/""".r

   def block = rep1sep(expr | ignored_line, "\n") ^^ {l => BlockE(l.toArray)} //.filterNot(EmptyE ==))}

   def ignored_line = ("""^//[^\n]*\n""".r | "") ^^^ EmptyE

   def expr: Parser[Expr] = java_code | assign //| evaluate //rec_assign

   def java_code = type_declaration ~ ("""\*/""".r ~> """[^/\*]""".r <~ """/\*""".r) ^^ {
      case dec_type ~ code => ApplyE(TypeE(dec_type), ListE(code.toArray map CharE))
   }

   def type_declaration = "'" ~> explicit_type <~ "'"

   def explicit_type: Parser[TypeOperator] = list_type | "boolean" ^^^ TypeSystem.BooleanT | "number" ^^^ TypeSystem.NumberT

   //| "character" ^^^ CharacterT | "empty" ^^^ EmptyT | "bo" ^^^ BooleanT | "nu" ^^^ NumberT | "ch" ^^^ CharacterT | "em" ^^^      EmptyT
   def list_type: Parser[TypeOperator] = "[" ~> explicit_type <~ "]" ^^ {type_expr => TypeSystem.ListType(type_expr)}

   def code = """[^/\*]"""

   def assign = identifier ~ ("=" ~> evaluate) ^^ AssignE

   //   def rec_assign = identifier ~ type_declaration ~ ("=" ~> (evaluate)) ^^ {
   //      case i ~ t ~ e =>
   //         val a = LetrecE(i, )
   //         opt match {
   //            case None => EmptyT
   //            case t => a.t = t.get
   //         }
   //         a
   //   }

   def evaluate = lambda * ("|" ^^^ ApplyE)

   def lambda: Parser[Expr] = ("\\\\" ~> (identifier)) ~ expr ^^ LambdaE | equality

   //   def equality = sum * ("==" ^^ {case ApplyE(ApplyE(IdentE("=="), _), _)}) // | "!=" ^^^ DiffE | ">=" ^^^ GreaterEqual | "<=" ^^^ LesserEqual | ">" ^^^ Greater | "<" ^^^ Lesser)
   //
   //   def sum = list_concatenation * ("+" ^^^ Add | "-" ^^^ Sub)
   //
   //   def list_concatenation = product * ("++" ^^^ ConcatenateListExpr)
   //
   //   def product = power * ("*" ^^^ Mul | "/" ^^^ Div | "%" ^^^ Resto)
   //
   //   def power = applicationlamb * ("^" ^^^ Pow)

   def equality = sum ~ rep("==" ~ sum | "!=" ~ sum | ">=" ~ sum | "<=" ~ sum | ">" ~ sum | "<" ~ sum) ^^ {
      case number ~ list =>
         (number /: list) {
            case (acc, op ~ nextNum) => ApplyE(ApplyE(IdentE(op), acc), nextNum)
         }
   }

   def sum = product ~ rep((not("`") ~> "+" ~ product | "-" ~ product)) ^^ {
      case number ~ list =>
         (number /: list) {
            case (acc, op ~ nextNum) => ApplyE(ApplyE(IdentE(op), acc), nextNum)
         }
   }

   def product = power ~ rep("*" ~ power | "/" ~ power | "%" ~ power) ^^ {
      case number ~ list =>
         (number /: list) {
            case (acc, op ~ nextNum) => ApplyE(ApplyE(IdentE(op), acc), nextNum)
         }
   }

   def power = application ~ rep("^" ~ application) ^^ {
      case number ~ list =>
         (number /: list) {
            case (acc, op ~ nextNum) => ApplyE(ApplyE(IdentE(op), acc), nextNum)
         }
   }

   def applicationlamb = application ~ lambda ^^ ApplyE | application

   def application = atomic_expr * ("" ^^^ ApplyE)

   //   var cc = -1
   //
   //   def composition: Parser[Expr] = {
   //      rep1sep(atomic_expr, not("..") ~> ".") ^^ {
   //         _.reduceRight(
   //            (a, b) => {cc += 1; LambdaE("§" + (cc + 64).toChar, ApplyE(a, ApplyE(b, Ident("§" + (cc + 64).toChar))))})
   //      }
   //   }

   lazy val identifier = mutable_ident | ident
   lazy val mutable_ident = """\$[a-zA-Z_]\w*""".r
   lazy val boolean = ("true" | "false") ^^^ BooleanE

   def parse_string(s: String) = {
      //      println(">" + s + "<")
      val slices = s.split('\'')
      if (slices.length < 2) {
         val l = slices.length match {
            case 0 => ListE(Array())
            case 1 => ListE(s.toArray map CharE)
         }
         //         exprs.t = HindleyDamasMilner.CharT
         //         l.t = ListT(CharT)
         l
      } else {
         if (slices.length % 2 == 0) failure("Unmatched ' inside string '" + s + "'.")
         val list_of_strings = slices.zipWithIndex map {
            case (sl, i) =>
               if (i % 2 == 0) {
                  val str = ListE(sl.toArray map CharE)
                  //                  str.t = ListT(CharT)
                  str
               }
               else {
                  //                  val ev = Eval
                  //                  ev.t = FunctionT(ListT(CharacterT), ListT(CharacterT))
                  //                  val str = ListExpr(("<< (" + sl + ")").toCharArray map CharacterExpr)
                  //                  str.t = ListT(CharacterT)
                  //                  ApplyE(ev, str)
                  EmptyE
               }
         }
         val l = list_of_strings.mkString //reduce (ConcatenateListExpr)
         //         l.t = ListT(CharacterT)
         l
      }
   }

   def transform(s: String) = s.replace("\\n", "\n").replace("\\”", "”").replace("\\\"", "\"")

   def atomic_expr: Parser[Expr] = {
      (
         //         anon //identifier engloba o simbolo _
         //            "<<" ^^^ Show
         identifier ^^ {x => IdentE(x)}
            | block
            //            | "[" ~> (sum(h) <~ "..") ~! sum(h) <~ "]" ^^ ListInterval
            //            | lista(h)
            //            | """-?(\d+(\.\d+)?)""".r ^^ {x => NumberExpr(x.toDouble)}
            //            | ("\"" | "“") ~> simple_string <~ ("\"" | "”") ^^ {case s => parse_string(transform(s))}
            //            | not_parseable_string
            //            | boolean
            //            | inversor
            //            | shell(h)
            //            | arg
            //            | empty
            //            | "`" ^^^ PrintLnE | "`+" ^^^ PrintE
            //            | "@" ^^^ Takehead | "~" ^^^ Taketail | "!" ^^^ Reverse
            //            | ("\'" | "‘") ~> simple_character <~ ("\'" | "’") ^^ {x => CharacterExpr(transform(x).head)}
            //            | ("'" ~> declared_type <~ "'") ~ (("""\*/""".r ~> """((?!/\*).)+""".r) <~ """/\*""".r) ^^ {case t ~ str => val sc = Scalacode(str); sc.t = t; sc}
            //            | "'" ~> declared_type <~ "'" ^^ {case t => val e = Eval; e.t = FunctionT(ListT(CharacterT), t); e}
            | ("" ~! "") ~> failure("expression expected...")
         )
   }

   //   def eval(h: Has_) = ("'" ~> declared_type <~ "'") ~ application(h) ^^ {
   //      case t ~ a =>
   //         val e = Eval
   //         e.t = FunctionT(ListT(CharacterT), t)
   //         ApplyE(e, a)
   //   } | application(h)

   def simple_character = """([^"^”^'^’^\\]|\\[\\'"n])""".r

   def simple_string = """([^"^”^\\]|\\[\\'"n])*""".r
}
