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

  private def ROUND = regexMatch("""(Round: |(Flight|Heat)  \d)?\s*?(Preliminaries|Semi-Finals|Finals)""".r) ^^ { case m => (m.group(3))  }

  private def PLACING = regexMatch("""^ {0,2}(\d{1,3}|-{2,3}) (\D+,\D+ ?\D+?)? (1[0-9])?([^\d]+)?\s{3,}""".r) ^^
    { case m => (m.group(1), m.group(2).trim(), m.group(3), m.group(4))}


  private def PRELIM_PLACING = regexMatch("""^ {0,2}(\d{1,3}|-{2,3}) (\D+,\D+ ?\D+?)? ([1-2][0-9])?([^\d]+)?\s{2,}((\d{1,2}?:?\d{0,2}\.\d{1,2})(m)?)?\s+((\d{1,2}?:?\d{0,2}\.\d{1,2}|ND|NH|DNS|DNF|FOUL)(m)?)(q|Q)?""".r) ^^
    { case m => (m.group(1), m.group(2).trim(), m.group(3), m.group(4), m.group(5), m.group(8), m.group(11))}


  private def HEAT = regexMatch("""(((Flight|Heat) {1,3}\d{1,2}) )""".r) ^^ { case m => m.group(1)  }

  def title(str:String): String ={
    return str.toLowerCase.capitalize
  }
}



class PlacingParser extends GroupedRegexParser{

}

object ChampsParser extends ChampsParser{

  var events:mutable.ListBuffer[Event] = new mutable.ListBuffer[Event]()
  var heat:String = ""


  def main(args: Array[String]) {

    val champsSource = Source.fromFile("data/champs2011.txt")
    val round:Any = None
    var heat:Any = None

    for (line <- champsSource.getLines()){
      parse(PLACING,line) match{
        case Success(matched,_) => {
          parsePlacing(events.last, line)
        }
        case Failure(msg,_) => parseEvent(line)
      }
    }
    //println( events )
  }

  def parsePlacing(event:Event, line:String): Unit ={
    event.round  match {
      case "Preliminaries" => parsePrelimPlacing(event, line)
      case "Semi-Finals" => parsePrelimPlacing(event, line)
      case "Finals"  => parseFinalsPlacing(event, line)
      case _ => println("No match for round " )
    }
  }

  def parsePrelimPlacing(event:Event, line:String): Unit ={
    parse(PRELIM_PLACING, line) match {
      case Success(matched,_) => println(matched)
      case Failure(msg,_) => println("Prelim No match")
    }
  }

  def parseFinalsPlacing(event:Event, line:String): Unit ={
    //println("")
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
        case Success(matched, _) => heat = matched
        case Failure(msg,_) => None
      }
    }

  }
}