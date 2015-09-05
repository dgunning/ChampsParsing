package champs.parsers


import java.io.{PrintWriter, File}

import champs.{Placing, Heat, Event}
import scala.collection.mutable
import scala.io.Source


/**
 * Created by dwight on 08/08/15.
 */
class ChampsParser extends GroupedRegexParser{

  private def EVENT   = regexMatch("""(Event \d{1,2}) *(Boys|Girls) (\d{1,2}-\d{1,2}) (\w+ \D+( \D+)?) (CLASS \d|OPEN)""".r ) ^^
    { case m => (new Event(m.group(4), m.group(2),  title(m.group(6)), m.group(1) )) }

  private def MULTI_EVENT = regexMatch("""(Event \d{1,2})  ((Decathlon|Heptathlon): #\d{1,2}) *(Boys|Girls) (\d{1,2}-\d{1,2}) (\w+ \D+( \D+)?)""".r) ^^
    { case m => (new Event(m.group(6) + " " + m.group(3), m.group(4),  "Open", m.group(1) )) }

  private def ROUND = regexMatch("""(Round: |(Flight  \d))?\s*?(Preliminaries|Semi\-Finals|Finals)""".r) ^^
    { case m => (m.group(2), m.group(3))  }

  private def PLACING = regexMatch("""^ {0,2}(\d{1,3}|-{2,3}) (\D+,\D+ ?\D+?)? (1[0-9])?([^\d]+)?\s{3,}((\d{1,2}?:?\d{0,2}\.\d{1,2})(m)?)?\s+((\d{1,2}?:?\d{0,2}\.\d{1,2}|ND|NH|DNS|DNF|FOUL)(m)?)\s*(\-?\d\.\d)?\s*(\d{1,2})?""".r) ^^
    { case m => mutable.Map[String,String]("rank"->m.group(1), "athlete"-> parseAthlete(m.group(2)),
                                  "age" -> m.group(3),"school" -> getSchool(m.group(4)), "seed" -> m.group(5),
                                  "mark" -> m.group(8), "wind"-> m.group(11), "points"-> m.group(12))
    }

  private def RELAY_PLACING =regexMatch("""^ {0,2}(\d{1,3}|-{2,3}) ([^\d]+)\s{10,}((\d:)?\d{2}\.\d{2})? *(((\d:)?\d{2}\.\d{2})|DQ|DNS|DNF)(Q|q)?(\s*?(\d{1,2}))?""".r) ^^{
    case m => (m, mutable.Map[String,String]("rank"-> m.group(1), "school"-> getSchool(m.group(2)), "seed" -> m.group(3), "mark"-> m.group(5),
                                "qualifies" -> m.group(8)))
  }
  private def PRELIM_PLACING = regexMatch("""^ {0,2}(\d{1,3}|-{2,3}) (\D+,\D+ ?\D+?)? ([1-2][0-9])?([^\d]+)?\s{2,}((\d{1,2}?:?\d{0,2}\.\d{1,2})(m)?)?\s+((\d{1,2}?:?\d{0,2}\.\d{1,2}|ND|NH|DNS|DNF|FOUL)(m)?)(q|Q)?""".r) ^^
    { case m =>  mutable.Map[String,String]("rank" -> m.group(1), "athlete" -> parseAthlete(m.group(2)), "age" ->m.group(3),
                    "school" -> getSchool(m.group(4)), "seed" -> m.group(5), "mark" -> m.group(8), "qualifies" -> m.group(11))
    }

  private def HEAT = regexMatch("""(Heat  \d{1,2}) (Preliminaries|Semi\-Finals)\s*(Wind: (\-?\d\.\d))?""".r) ^^
                        { case m => (new Heat( m.group(1), m.group(2), m.group(4) ) ) }

  def title(str:String): String ={
    return str.toLowerCase.capitalize
  }

  def parseAthlete(compName:String):String ={
    compName.split(",").map(n => n.trim()).reverse.mkString(" ")
  }

  def getSchool(team:String):String ={
    team.trim()
  }
}



object ChampsParser extends ChampsParser{

  var events:mutable.ListBuffer[Event] = new mutable.ListBuffer[Event]()
  var heat:Heat = null

  def main(args: Array[String]) {

    val champsSource = Source.fromFile("data/champs2011.txt")

    for (line <- champsSource.getLines()){
      parse(PLACING,line) match{
        case Success(matched,_) => {
          parsePlacing(events.last, line)
        }
        case Failure(msg,_) => parseEvent(line)
      }
    }

    writeCsv()
  }

  def writeCsv(): Unit ={
    val writer = new PrintWriter(new File("data/champs2011.csv" ))
    val headers:List[String] = List("rank", "athlete", "age", "school", "eventName", "eventCategory", "round", "seed", "mark", "wind", "points")
    writer.write(headers.mkString(","))
    writer.write("\n")
    for (event <- events){
      for( placing <- event.placings){
          placing.put("eventName",  event.eventName )
          placing.put("eventCategory",event.eventCategory)
          placing.put("eventType", event.eventType)
          writer.write( headers.map( h => placing.getOrElse(h, "NA")).mkString(",") )
          writer.write("\n")
      }
    }
    writer.close()
    //println( events )
  }

  def parsePlacing(event:Event, line:String): Unit ={
    event.round  match {
      case "Preliminaries" => parsePrelimPlacing(event, line)
      case "Semi-Finals" => parsePrelimPlacing(event, line)
      case "Finals"  => parseFinalsPlacing(event, line)
      case _ => println("No match " + line )
    }
  }

  def parsePrelimPlacing(event:Event, line:String): Unit ={
    parse(PRELIM_PLACING, line) match {
      case Success(matched,_) => events.last.placings += matched
      case Failure(msg,_) => {}
    }
  }

  /**
   *
   */
  def parseRelayPlacing(event:Event, line:String): Unit ={
      parse(RELAY_PLACING, line) match{
        case Success(matched, _) =>{
          event.round match{
            case "Preliminaries" =>  events.last.placings += matched._2 + ("heat" ->  matched._1.group(10) )
            case "Finals" => events.last.placings += matched._2 + ("points" ->  matched._1.group(10) )
          }
        }
        case Failure(msg,_) => {}//println("Relay No match " + line)}
      }
  }

  def parseFinalsPlacing(event:Event, line:String): Unit ={
    parse( PLACING , line ) match {
      case Success(matched,_) =>{
        events.last.placings += matched
      }
      case Failure(msg,_) => println("Finals No match " + line)
    }
  }

  def parseEvent(line:String): Unit ={
      parse(EVENT, line) match{
        case Success(matched,_) => {
          heat = null
          events += matched
        }
        case Failure(msg,_) => parseMultiEvent(line)
        case Error(msg,_) => println(msg)
    }

    def parseMultiEvent(line:String): Unit ={
      parse(MULTI_EVENT, line) match{
        case Success(matched,_) => {
          heat = null
          events += matched
        }
        case Failure(msg,_) => parseRound(line)
        case Error(msg,_) => println(msg)
      }
    }

    def parseRound(line:String): Unit ={
      parse(ROUND, line) match {
        case Success(matched, _) => {
          events.last.round = matched._2
          if( matched._1 != null ) heat = new Heat(matched._1, matched._2, null)
        }
        case Failure(msg, _) => parseHeat(line)
        case Error(msg,_) => println(msg)
      }
    }

    def parseHeat(line:String): Unit = {
      parse(HEAT, line) match{
        case Success(matched, _) => {
          heat = matched
          events.last.round = heat.round
        }

        case Failure(msg,_) => parseRelayPlacing(events.last, line)
      }
    }


  }
}