package champs
import PartialFunction._

/**
 * Created by dwight on 08/08/15.
 */
class Event(eventName:String, eventGender:String, eventClass:String, eventNumber:String) {

  var round  =""
  var eventCategory:String = Event.getCategory(eventName)
  var eventType = Event.getEventType(eventName)

  override def toString(): String ="("+ round + "," + eventGender +","+ eventClass +","+ eventName + "," + eventCategory + "," + eventType +")";
}

object Event {

 def onlyString(v: Any): Option[String] = condOpt(v) { case x: String => x }

  def getCategory(eventName:String): String ={
     EVENTS.get(eventName) match{
      case Some(desc) => return onlyString(desc.get("category").get).get
      case None => return "NA"
     }
  }

 def getEventType(eventName:String):String ={
  EVENTS.get(eventName) match{
   case Some(desc) => return onlyString(desc.get("type").get).get
   case None => return "NA"
  }
 }
  def describeEvent(eventName:String): Option[Map[String,Any]] ={
    return EVENTS.get(eventName)
  }

  val EVENTS:Map[String,Map[String,Any]] =
   Map("100 Meter Dash"->Map("category"->"Track","type"->"Sprint","hasWind"-> true  ),
 "200 Meter Dash" ->Map("category"->"Track","type"->"Sprint","hasWind"-> true ),
 "400 Meter Dash" ->Map("category"->"Track","type"->"Sprint"),
 "800 Meter Run" ->Map("category"->"Track","type"->"Distance"),
 "1500 Meter Run" ->Map("category"->"Track","type"->"Distance"),
 "2000 Meter Steeplechase"->Map("category"->"Track","type"->"Distance"),
 "3000 Meter Run"->Map("category"->"Track","type"->"Distance"),
 "5000 Meter Run"->Map("category"->"Track","type"->"Distance"),
 "70 Meter Hurdles"->Map("category"->"Track","type"->"Hurdles","hasWind"-> true),
 "80 Meter Hurdles"->Map("category"->"Track","type"->"Hurdles","hasWind"-> true),
 "100 Meter Hurdles" ->Map("category"->"Track","type"->"Hurdles","hasWind"-> true ),
 "110 Meter Hurdles"->Map("category"->"Track","type"->"Hurdles","hasWind"-> true),
 "400 Meter Hurdles" ->Map("category"->"Track","type"->"Hurdles"),
 "4 X100 Meter Relay" ->Map("category"->"Track","type"->"Relay"),
 "4 X400 Meter Relay" ->Map("category"->"Track","type"->"Relay"),
 "1600 Sprint Medley"->Map("category"->"Track","type"->"Relay"),
 "High Jump"->Map("category"->"Field","type"->"Jumps"),
 "Long Jump"->Map("category"->"Field","type"->"Jumps","hasWind"-> true),
 "Triple Jump"->Map("category"->"Field","type"->"Jumps","hasWind"-> true),
 "Pole Vault"->Map("category"->"Field","type"->"Jumps"),
 "Shot Put"->Map("category"->"Field","type"->"Throws"),
 "Discus Throw"->Map("category"->"Field","type"->"Throws"),
 "Javelin Throw"->Map("category"->"Field","type"->"Throws"),
 "Heptathlon"->Map("category"->"Mixed","type"->"Heptathlon"),

 "100 Meter Hurdles Heptathlon"->Map("category"->"Track","type"->"Sprint","hasWind"-> true),
 "200 Meter Run Heptathlon"->Map("category"->"Track","type"->"Sprint","hasWind"-> true),
 "800 Meter Run Heptathlon"->Map("category"->"Track","type"->"Distance"),
 "Javelin Throw Heptathlon"->Map("category"->"Field","type"->"Throws"),
 "Shot Put Heptathlon"->Map("category"->"Field","type"->"Throws"),
 "High Jump Heptathlon"->Map("category"->"Field","type"->"Jumps"),
 "Long Jump Heptathlon"->Map("category"->"Field","type"->"Jumps","hasWind"-> true),

 "Decathlon"->Map("category"->"Mixed","type"->"Decathlon"),

 "100 Meter Run Decathlon"->Map("category"->"Track","type"->"Sprint","hasWind"-> true),
 "110 Meter Hurdles Decathlon"->Map("category"->"Track","type"->"Hurdles","hasWind"-> true),
 "400 Meter Dash Decathlon"->Map("category"->"Track","type"->"Sprint"),
 "1500 Meter Run Decathlon"->Map("category"->"Track","type"->"Distance"),
 "Javelin Throw Decathlon"->Map("category"->"Field","type"->"Throws"),
 "Pole Vault Decathlon"->Map("category"->"Field","type"->"Jumps"),
 "Shot Put Decathlon"->Map("category"->"Field","type"->"Throws"),
 "Discus Throw Decathlon"->Map("category"->"Field","type"->"Throws"),
 "High Jump Decathlon"->Map("category"->"Field","type"->"Jumps"),
 "Long Jump Decathlon"->Map("category"->"Field","type"->"Jumps","hasWind"-> true)
  )

}


