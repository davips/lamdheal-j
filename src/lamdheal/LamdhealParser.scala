package lamdheal

import util.parsing.combinator.{JavaTokenParsers, ImplicitConversions, RegexParsers}

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
object LamdhealParser extends RegexParsers with ImplicitConversions with JavaTokenParsers {
   def parse(text: String): Expr = {
      val u = parseAll(program, texto)
      var line = 0
      u match {
         case Success(t, next) => {
            if (!web) u.get.l map {
               x =>
                  if (x != EmptyExpr) println(line + ": " + x.dressed_string)
                  line += 1
            }
            u.get
         }
         case f => {
            val location = f.toString.split(": ").head.split('.').head.tail
            val error = "at line " + location + ": " + f.toString.split(": ").last
            var str = error
            if (error.contains("expected but `\"' found")) str += "\nHint: strings, like in Java, have strict rules around the characters '\\' and, obviously, " +
               "'\"'. '\\' normally is followed by one of the characters: 't', 'n', '\"', '\\' and 'r'; or it can end in strange parsing errors."
            throw new ParserException(str)
         }
      }
   }

}
