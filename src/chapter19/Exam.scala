package chapter19

import scala.util.parsing.combinator.RegexParsers

/**
  * Created by gujd on 2017/1/16.
  */

class ExprParser extends RegexParsers {
    val number = "[0-9]+".r

    def expr: Parser[Any] = term   ~ opt(("+" | "-") ~ expr)
    def term: Parser[Any] = factor ~ rep ("*" ~ factor)
    def factor: Parser[Any] = number | "(" ~ expr ~ ")"
}

class ExprParserToInt extends RegexParsers {
    val number = "[0-9]+".r

    def expr: Parser[Int] = term  ~ opt(("+" | "-") ~ expr) ^^ {
        case t ~ None => t
        case t ~ Some("+" ~ e) => t + e
        case t ~ Some("-" ~ e) => t - e
    }
    def term: Parser[Int] = factor ~ rep ("*" ~ factor) ^^ {
        case f ~ r => f * r.map(_._2).product
    }
    def factor: Parser[Int] = number ^^ {_.toInt} | "(" ~ expr ~ ")" ^^ {
        case _ ~ e ~ _ => e
    }
}

//Ex: 2
class ExprParserToInt2 extends RegexParsers {
    val number = "[0-9]+".r

    def expr: Parser[Int] = term  ~ ((("+" | "-") ~ expr)?) ^^ {
        case t ~ None => t
        case t ~ Some("+" ~ e) => t + e
        case t ~ Some("-" ~ e) => t - e
    }
    def term: Parser[Int] = factor ~  rep(("*"|"/"|"%") ~ factor) ^^ {
        case f ~ r => {
            r.foldLeft(f)((acc, k) => k._1 match {
                case "*" => acc * k._2
                case "/" => acc / k._2
                case "%" => acc % k._2
            })
        }
    }
    def factor: Parser[Int] = number ^^ {_.toInt} | "(" ~> expr <~ ")"
}


class Expr
case class Number(value: Int) extends Expr
case class Operator(op: String, left: Expr, right: Expr) extends Expr

class ExprParserToExpr extends RegexParsers {
    val number = "[0-9]+".r

    def expr: Parser[Expr] = term  ~ ((("+" | "-") ~ expr)?) ^^ {
        case t ~ None => t
        case t ~ Some("+" ~ e) => Operator("+", t, e)
        case t ~ Some("-" ~ e) => Operator("-",t, e)
    }
    def term: Parser[Expr] = (factor ~  opt("*" ~> factor)) ^^ {
        case a ~ None => a
        case a ~ Some(b) => Operator("*", a, b)
    }
    def factor: Parser[Expr] = number ^^ {n => Number(n.toInt)} | "(" ~> expr <~ ")"
}

class Parser1 extends RegexParsers {
//    def ones: Parser[Any] = "1" ~ ones //X

//    def ones: Parser[Any] = "1" ~ ones | "1" // ok!
//    def ones: Parser[Any] = rep1("1")        // ok!

}

//Ex: 3
class IntListParser extends RegexParsers {
    val number = "[0-9]+".r

    val numseq:Parser[List[Int]] = "(" ~> repsep(number, ",")  <~ ")" ^^ {
        n => n.map(_.toInt)
    }
}


object TestExam {
    def main(args: Array[String]) {
//        val parser = new ExprParserToInt
//        val result = parser.parseAll(parser.expr, "3-4*5")
//        if(result.successful) {
//            println(result.get)
//        }
//        val parser = new IntListParser
//        val result = parser.parseAll(parser.numseq, "(3,4)")
//        if(result.successful) {
//            println(result.get)
//        }

        val parser = new ExprParserToInt2
        val result = parser.parseAll(parser.expr, "1*2+3*4*5")
        if(result.successful) {
            println(result.get)
        }
    }
}
