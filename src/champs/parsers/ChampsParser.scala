package champs.parsers


import champs.Event

import scala.collection.mutable
import scala.io.Source


/**
 * Created by dwight on 08/08/15.
 */
class ChampsParser extends GroupedRegexParser{

  private def EVENT   = regexMatch("""(Event \d{1,2}) *(Boys|Girls) (\d{1,2}-\d{1,2}) (\w+ \D+( \D+)?) (CLASS \d|OPEN)""".r ) ^^
    { case m => (new Event(m.group(4), m.group(2),  title(m.group(6)), m.group(1) )) }

  private def ROUND = regexMatch("""(Preliminaries)""".r) ^^ { case m => (m.group(1))  }

  private def PLACING = regexMatch("""^ {0,2}(\d{1,3}|-{2,3}) (\D+,\D+ ?\D+?)? (1[0-9])?([^\d]+)?\s{3,}""".r) ^^
    { case m => (m.group(1), m.group(2).trim(), m.group(3), m.group(4))}

  private def HEAT = regexMatch("""(((Flight|Heat) {1,3}\d{1,2}) )""".r) ^^ { case m => m.group(1)  }

  def title(str:String): String ={
    return str.toLowerCase.capitalize
  }
}


object ChampsParser extends ChampsParser{

  var events:mutable.ListBuffer[Event] = new mutable.ListBuffer[Event]()
  var heat:String = ""


  def main(args: Array[String]) {

    val champsSource = Source.fromFile("data/champs2011.txt")
    var round:Any = None

    for (line <- champsSource.getLines()){
      parse(PLACING,line) match{
        case Success(matched,_) => { println(matched) }
        case Failure(msg,_) => parseEvent(line)
      }
    }
    //println( events )
  }


  def parseEvent(line:String): Unit ={
      parse(EVENT, line) match{
        case Success(matched,_) =>  events += matched
        case Failure(msg,_) => parseRound(line)
        case Error(msg,_) => println(msg)
    }

    def parseRound(line:String): Unit ={
      parse(ROUND, line) match {
        case Success(matched, _) => events.last.round = matched
        case Failure(msg, _) => parseHeat(line)
        case Error(msg,_) => println(msg)
      }
    }

    def parseHeat(line:String): Unit = {
      parse(HEAT, line) match{
        case Success(matched, _) => println(matched)
        case Failure(msg,_) => None
      }
    }

  }
}