package lamdheal

import util.parsing.combinator.{JavaTokenParsers, ImplicitConversions, RegexParsers}
import lamdheal.TypeSystem.{ListT, Type}

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
object Parsing extends RegexParsers with ImplicitConversions with JavaTokenParsers {
   /**
    * Converts a text of code into an AST.
    * @param text source code
    * @return AST
    */
   def parse(text: String): Expr = {
      val u = parseAll(program, text.replace("\r\n", "\n").replace("\r", "\n").replace("\n", ";\n"))
      u match {
         case Success(t, next) => {
            val ug = u.get
//            ug.l map println
            ug
         }
         case f => {
            throw new Exception("" + f)
         }
      }
   }

   def program = """/\*""".r ~> block <~ """;\n\*/""".r

   def block = rep1sep(expr, ";") ^^ {l => BlockE(l.toArray)} //.filterNot(EmptyE ==))}

   def expr: Parser[Expr] = ignored_line | assign | evaluate //rec_assign

   def ignored_line = ("""//.*(?=;)""".r | """\s*(?=;)""".r) ^^^ EmptyE

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

   def evaluate = (lambda | equality) * ("|" ^^^ ApplyE)

   def lambda: Parser[Expr] = ("\\\\" ~> (identifier)) ~ expr ^^ LambdaE

   def equality = sum ~ rep("==" ~ sum | "!=" ~ sum | ">=" ~ sum | "<=" ~ sum | ">" ~ sum | "<" ~ sum) ^^ {
      case number ~ list =>
         (number /: list) {
            case (acc, op ~ nextNum) => ApplyE(ApplyE(IdentE("(" + op + ")"), acc), nextNum)
         }
   }

   def sum = product ~ rep(not("`") ~> "+" ~ product | "-" ~ product) ^^ {
      case number ~ list =>
         (number /: list) {
            case (acc, op ~ nextNum) => ApplyE(ApplyE(IdentE("(" + op + ")"), acc), nextNum)
         }
   }

   def product = power ~ rep("*" ~ power | "/" ~ power | "%" ~ power) ^^ {
      case number ~ list =>
         (number /: list) {
            case (acc, op ~ nextNum) => ApplyE(ApplyE(IdentE("(" + op + ")"), acc), nextNum)
         }
   }

   def power = application ~ rep("^" ~ application) ^^ {
      case number ~ list =>
         (number /: list) {
            case (acc, op ~ nextNum) => ApplyE(ApplyE(IdentE("(" + op + ")"), acc), nextNum)
         }
   }

   def applicationlamb = application ~ lambda ^^ ApplyE | application

   def application = composition * ("" ^^^ ApplyE)

   var cc = -1

   def composition: Parser[Expr] = {
      rep1sep(atomic_expr, not("..") ~> ".") ^^ {
         _.reduceRight(
            (a, b) => {cc += 1; LambdaE("§" + (cc + 64).toChar, ApplyE(a, ApplyE(b, IdentE("§" + (cc + 64).toChar))))})
      }
   }

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
         "<<" ^^^ IdentE("<<")
            | identifier ^^ {x => IdentE(x)}
            | java_code
            | "(" ~> block <~ ")"
            //            | "[" ~> (sum(h) <~ "..") ~! sum(h) <~ "]" ^^ ListInterval
            | list
            | """-?(\d+(\.\d+)?)""".r ^^ {x => NumberE(x)}
            //            | ("\"" | "“") ~> simple_string <~ ("\"" | "”") ^^ {case s => parse_string(transform(s))}
            //            | not_parseable_string
            //            | boolean
            //            | inversor
            //            | shell(h)
            //            | arg
            //            | empty
            | "`" ^^^ IdentE("`") | "`+" ^^^ IdentE("`+")
            //            | "@" ^^^ Takehead | "~" ^^^ Taketail | "!" ^^^ Reverse
            //            | ("\'" | "‘") ~> simple_character <~ ("\'" | "’") ^^ {x => CharacterExpr(transform(x).head)}
            //            | ("'" ~> declared_type <~ "'") ~ (("""\*/""".r ~> """((?!/\*).)+""".r) <~ """/\*""".r) ^^ {case t ~ str => val sc = Scalacode(str); sc.t = t; sc}
            //            | "'" ~> declared_type <~ "'" ^^ {case t => val e = Eval; e.t = FunctionT(ListT(CharacterT), t); e}
            | ("" ~! "") ~> failure("expression expected...")
         )
   }

   //   def run(h: Has_) = ("'" ~> declared_type <~ "'") ~ application(h) ^^ {
   //      case t ~ a =>
   //         val e = Eval
   //         e.t = FunctionT(ListT(CharacterT), t)
   //         ApplyE(e, a)
   //   } | application(h)

   def java_code = type_declaration ~ ("""\*/(?:.|[\n\r])+?/\*""".r) ^^ {
      case dec_type ~ code =>
         ApplyE(TypeE(dec_type), ListE(code.toArray.drop(2).dropRight(2) map CharE))
   }

   def type_declaration = "'" ~> explicit_type <~ "'"

   def explicit_type: Parser[Type] = list_type | "boo" ^^^ TypeSystem.BooleanT | "num" ^^^ TypeSystem.NumberT | "cha" ^^^ TypeSystem.CharT | "emp" ^^^ TypeSystem.EmptyT

   def list_type: Parser[Type] = "[" ~> explicit_type <~ "]" ^^ {type_expr => TypeSystem.ListT(type_expr)}

   def simple_character = """([^"^”^'^’^\\]|\\[\\'"n])""".r

   def simple_string = """([^"^”^\\]|\\[\\'"n])*""".r

   def list = n_list | empty_list

   def empty_list = "[" ~> type_declaration <~ "]" ^^ {
      type_expr =>
         val list = ListE(Array[Expr]())
         list.t = ListT(type_expr)
         list
   } | "[" ~! "]" ^^^ {
      failure("EmptyExpr lists must have an explicitly defined type." +
         "Examples of valid code: \"exprs = ['boo']\", \"exprs = ['num']\" or " +
         "\"exprs = ['cha']\".\nNote that a list filled with empty lists is not empty.")
      EmptyE //Just to satisfy Parser sexual typing needs.
   }

   def n_list = "[" ~> rep1sep(expr, ",") <~ "]" ^^ {
      exs => ListE(exs.toArray)
   }
}
