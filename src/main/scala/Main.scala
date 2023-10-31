package pp202302.assign2

import scala.annotation.tailrec
import scala.util.control.TailCalls._

/** Principles of Programming: Assignment 02.
  *
  * Implement given functions, which are currently left blank. (???) **WARNING:
  * Please read the restrictions below carefully.**
  *
  * If you do not follow these, **your submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `List`,
  *     `Array`, `Range` (`1 to n`, `1 until n` ...), `fold`, `map`, `reduce` or
  *     etc.
  *   - If you want to use a data structure, create new one instead of using the
  *     library ones.
  *   - You can only use tuples, `scala.annotation.tailrec`, and
  *     `scala.util.control.TailCalls._`, `Math._` from the library.
  *   - Do not use any looping syntax of Scala (`for`, `while`, `do-while`,
  *     `yield`, ...)
  *
  * Again, your score will be zero if you do not follow these rules.
  *
  * Note that these rules will be gradually relaxed through the next
  * assignments.
  *
  * We do not require tail-recursion explicitly for this assignment.
  *
  * Timeout: 30 sec.
  */
object Assignment2:
  import Expr.*
  import PEG.*
  import PEGFunc.*
  import IOption.*
  import IEither.*
  import IList.*

  /** Problem 1: Simple Arithmetic Calculator (10 Points)
    */
  def calculator(e: Expr): Long = 
  {
    e match {
      case Num(n) => n
      case Add(f1, f2) => calculator(f1) + calculator(f2)
      case Sub(f1, f2) => calculator(f1) - calculator(f2)
      case Div(f1, f2) => calculator(f1) / calculator(f2)
      case Mul(f1, f2) => calculator(f1) * calculator(f2)
    }
  }

  /** Problem 2: Parsing Expression Grammar.
    *
    * Parsing Expression Grammar (PEG) is one way to describe a grammar of
    * language, especially for programming languages.
    *
    * Starting from Python 3.9, Python uses PEG to describe its grammar and its
    * parsing speed was increased by 10%.
    *
    * Unlike CFG (Context-Free Grammar), all elements in PEG are
    * **deterministic** and **greedy**, so there is no ambiguity on parsing.
    *
    * See https://en.wikipedia.org/wiki/Parsing_expression_grammar
    */

  /** Problem 2-1: Consume PEG (30 points)
    *
    * Match prefix of the string `s` and return the remained suffix.
    *
    * ```
    * // e.g.)
    * consumePeg("ab" ~ "c"*, "abcccccdccc") == ISome("dccc")
    * consumePeg(!"a" ~ "bc", "bcd") == ISome("d")
    * consumePeg(!"a" ~ "bc", "abc") == INone
    * ```
    */ 
  def consumePeg(p: PEG, s: String): IOption[String] = {
    
        p match{
      
            case Str(comp) => {
                if (s.startsWith(comp)) ISome(s.replace(comp,""))
                else INone
            }
      
            case Cat(p1, p2) => {
                val t = consumePeg(p1,s) 
                t match {
                    case ISome(s2) => {
                    consumePeg(p2,s2)
                    }
                }
            }

            case OrdChoice(p1, p2) => {
                val t = consumePeg(p1,s)
                t match {
                    case ISome(s2) => t
                    case INone => consumePeg(p2,s)
                }}

            case NoneOrMany(p) => {
                val t = consumePeg(p,s)
                t match {
                    case INone => ISome(s)
                    case ISome(s2) => {
                        consumePeg(NoneOrMany(p),s2)
                    }
                }
            }

            case OneOrMany(p) => {
                val t = consumePeg(p,s)
                t match {
                    case INone => INone
                    case ISome(s2) => {
                        consumePeg(OneOrMany(p),s2)
                    }
                }
            }

            case Optional(p) => {
                val t = consumePeg(p,s)
                t match {
                    case INone => INone
                    case ISome(s2) => ISome(s2)
                }
            }

            case ExistP(p) => {
                val t = consumePeg(p,s)
                t match {
                    case INone => INone
                    case ISome(s2) => ISome(s)
                }
            }

            case NotP(p) => {
                val t = consumePeg(p,s)
                t match {
                    case INone => ISome(s)
                    case ISome(s2) =>	INone
                }
            }
            }}

  /** Problem 2-2: Match PEG (10 points)
    *  
    * Return true if the given string `s` matches with the given PEG `p`.
    * 
    * ```
    * // e.g.)
    * matchPeg("abc", "abcd") == false
    * matchPeg("a"+, "aaaa") == true
    * ```
    */
  def matchPeg(p: PEG, s: String): Boolean = {
        val t = consumePeg(p, s) 
        t match {
    case ISome(a) => {
            if (a == "") true 
            else false
        } 
    case INone => false
        }}

  /** Problem 2-3: Map PEG to value (25 points)
    * 
    * `PEGFunc[V]` is a PEG expression with expression mapper which returns a value 
    * with the type `V`.
    * 
    * e.g.) `FStr(s, f)` matches a string `s` and returns `f(s)`.
    * 
    * If `pf` matches with `s`, return `ISome(v)`, where `v` is the value
    * returned by internal function of `pf`. Otherwise, return `INone`.
    */
  def mapPeg[V](pf: PEGFunc[V], s: String): IOption[V] = {


    def mapPegRec[V](pf : PEGFunc[V], s: String): IOption[V] = {

            pf match {

            case FStr(s2,f) => {
                if (matchPeg(Str(s2),s)) ISome(f(s2)) else INone
            }
            case _ => INone
        }
    }
    mapPegRec(pf,s)

/*
        def toPEG[T] (pf2 : PEGFunc[T]): PEG = {
            pf2 match {
                case FStr(s,f) => Str(s)
                case FCat(p1,p2,f) => Cat(toPEG(p1),toPEG(p2))
                case FOrdChoice(p1,p2,f) => OrdChoice(toPEG(p1),toPEG(p2))
                case FNoneOrMany(p1,p2,f) => NoneOrMany(toPEG(p1),toPEG(p2))
                case FOneOrMany(p1,p2,f) => OneOrMany(toPEG(p1),toPEG(p2))
                case FOptional(p,f) => Optional(toPEG(p))
                case FExistP(p) => ExistP(toPEG(p))
                case FNotP(p) => NotP(toPEG(p))
                case _ => INone
                }
        }
        def mapPegRec[V](pf : PEGFunc[V], s: String): IOption[V] = {

            pf match {

            case FStr(s2,f) => {
                if (matchPeg(Str(s2),s)) ISome(f(s2)) else INone
            }

            case FCat(p1: PEGFunc[A], p2: PEGFunc[B], f) => {
                mapPegRec(p1,s) match {
                    case ISome(firstPEG) => {
                        val temp = consumePeg(toPEG(p1),s)
                        temp match {
                            case ISome(leftover) => {
                                case ISome(left) => val leftString = left
                                case _ => val leftString = ""
                            }
                            mapPegRec(p2,leftString) match {
                                case ISome(secondPEG) => ISome(f(firstPEG,secondPEG))
                                case INone => INone
                            }
                            case INone => INone
                        }
                    }
                }
            }
      
          case FOrdChoice[A, B, C](p1: PEGFunc[A], p2: PEGFunc[B], f) => {
            mapPegRec(p1,s) match {
                case ISome(firstPEG) => ISome(ILeft(firstPEG))
                case INone => {
                    mapPegRec(p2,s) match {
                        case ISome(secondPEG) => ISome(IRight(secondPEG))
                        case INone => INone
                    }
                }
            }
            }
            /* 
          case FNoneOrMany[A, B](p: PEGFunc[A], f) => {
            
            def ManyItr[A](p:PEGFunc[A], s:String, ls: IList):IList[A] => {
                mapPegRec(p,s) match {
                    case ISome(firstPEG) => {
                        val temp = consumePeg(toPEG(p1),s)
                        temp match {
                            case ISome(leftover) => {
                                case ISome(left) => val leftString = left
                                case _ => val leftString = ""
                                ManyItr(p,left,ICons(temp,ls))
                            }
                            case INone => ls
                        }
                    }

                } 
                
            }
            ISome(f(ManyItr(p,s,INil)))
            }
          case FOneOrMany[A, B](p: PEGFunc[A], f) => {
            if (matchPeg(OneOrMany(toPEG(p)), s)) ISome(f(p)) else INone
            }
          case FOptional[A, B](p: PEGFunc[A], f) => {
            if (matchPeg(Optional(toPEG(p)), s)) ISome(f(p)) else INone
            }
          case FExistP[A](p: PEGFunc[A]) => {
            if (matchPeg(ExistP(toPEG(p)), s)) consumePeg(toPEG(p)) else INone
            }
          case FNotP[A](p: PEGFunc[A]) => {
            if (matchPeg(NotP(toPEG(p)), s)) consumePeg(toPEG(p)) else INone
            }

        }
        }
        INone
        */
        
    
    }
}
*/}

  /** Problem 2-4: Arithmetic PEG (15 points)
    *
    * Write a PEG expression which extracts an arithmetic expression.
    * We allow leading 0s in the expressions, and all the numbers will be in the range of Long.
    * 
    * PEG expression of arithmetic expression looks like below.
    * ```
    * Expr = MulDiv ~ ("+" ~ MulDiv | "-" ~ MulDiv)*
    * MulDiv = Num ~ ("*" ~ Num | "/" ~ Num)*
    * Num = (0-9)+
    * ```
    */
val arithPeg: PEGFunc[Expr] = ???
