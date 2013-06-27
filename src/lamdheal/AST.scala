package lamdheal

/*  Copyright 2013 Davi Pereira dos Santos
    This file is part of Lamdheal.
    Initially written according to the guidelines in the Masterarbeit of Eugen Labun.

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
object ASTtypes {

   sealed abstract class ExprType {
   }

   case object EmptyT extends ExprType {
      override def toString = "'Ø'" //'empty'"
   }

   case object BooleanT extends ExprType {
      override def toString = "'boolean'"
   }

   case object NumberT extends ExprType {
      override def toString = "'number'"
   }

   case object CharacterT extends ExprType {
      override def toString = "'character'"
   }

   case class ListT(elements_type: ExprType) extends ExprType {
      override def toString = "'list of " + elements_type + "'"
   }

   case class FunctionT(from: ExprType, to: ExprType) extends ExprType {
      override def toString = "'" + from + " -> " + to + "'"
   }

//   case class Variable(id: Int) extends ExprType {
//      var instance: Option[ExprType] = None
//      lazy val name = HindleyDamasMilner.nextUniqueName
//   }

}

object AST {
   import lamdheal.ASTtypes._

   sealed abstract class Expr {
      def dressed_string = toString

      var t: ExprType = null
      //      var x = 0
      //      var y = 0
   }

}
