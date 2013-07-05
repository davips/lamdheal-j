package lamdheal

import lamdheal.TypeSystem.Type

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


sealed abstract class Expr

case class LambdaE(v: String, body: Expr) extends Expr

case class IdentE(name: String) extends Expr

case class ApplyE(fn: Expr, arg: Expr) extends Expr

case class LetE(v: String, defn: Expr, body: Expr) extends Expr

case class AssignE(v: String, expr: Expr) extends Expr

case class LetrecE(v: String, defn: Expr, body: Expr) extends Expr

case class NumberE(n: String) extends Expr
case class BooleanE(b: String) extends Expr

case class CharE(c: Char) extends Expr

case class ListE(l: Array[Expr]) extends Expr
case class BlockE(l: Array[Expr]) extends Expr

case class TypeE(t: Type) extends Expr
case object EmptyE extends Expr

object Expr {
   def string(ast: Expr): String = {
      if (ast.isInstanceOf[IdentE])
         nakedString(ast)
      else
         nakedString(ast)
   }

   def nakedString(ast: Expr): String = ast match {
      case i: IdentE => i.name
      case l: LambdaE => "fn " + l.v + " ⇒ " + string(l.body)
      case f: ApplyE => string(f.fn) + " " + string(f.arg)
      case l: LetE => "let " + l.v + " = " + string(l.defn) + " in " + string(l.body)
      case l: LetrecE => "letrec " + l.v + " = " + string(l.defn) + " in " + string(l.body)
      case l: AssignE => l.v + " = " + string(l.expr)
      case NumberE(n) => n
      case CharE(c) => c.toString
      case ListE(l) => '[' + l.map(nakedString).mkString(", ") + ']'
      case BlockE(l) => "(" + l.map(nakedString).mkString(", ") + ')'
      case EmptyE => "Ø"
   }
}
