import scala.io.Source
import scala.math
import scala.xml._


class Profile(line: String) {
  
  val data = line.split(",")
  val Descript = data(0).trim
  val EduRank = data(1).toInt
  val IncomeRank = data(2).toInt
  val Dem20sRank = data(3).toInt
  val DensityRank = data(4).toInt

  def Cnt = this.Descript + this.EduRank + this.IncomeRank + this.Dem20sRank + this.DensityRank


  def ZipScore(Locale: USLocale) =
     Locale.EduRank * this.EduRank +
     Locale.IncomeRank * this.IncomeRank +
     Locale.Dem20sRank * this.Dem20sRank +
     Locale.DensityRank * this.DensityRank


  
}


class USLocale(line: String) {
  
   val data = line.split(",")
   val Zip = data(0)
   val State = data(1).trim
   val Lat = data(2).toDouble
   val Lng = data(3).toDouble
   val City = data(4).trim
   
   val EduRank = data(18).toInt
   val IncomeRank = data(37).toInt
   val StabilityRank = data(24).toInt
   val Dem20sRank = data(54).toInt
   val DensityRank = data(6).toInt

   var Descript = ""
   
   override def toString = "Zip: " + Zip + " " + City + "," + State + " (" + Lat + "," + Lng + ")\n" + "Education: " + EduRank + " Income: " + 
                           IncomeRank + " Stability: " + StabilityRank + " In20s: " + Dem20sRank + " Density: " + DensityRank

   def SetDescript(descript: String) = Descript = descript


   def ProfileScore(Prof: Profile) =     
     Prof.EduRank * this.EduRank +
     Prof.IncomeRank * this.IncomeRank +
     Prof.Dem20sRank * this.Dem20sRank +
     Prof.DensityRank * this.DensityRank

   def MilesFrom(lat: Double, lng: Double) = {

	  	val degrees_to_radians = scala.math.Pi / 180.0
		val phi1 = (90.0 - lat) * degrees_to_radians
		val phi2 = (90.0 - this.Lat) * degrees_to_radians
		
		val theta1 = lng * degrees_to_radians
		val theta2 = this.Lng * degrees_to_radians

		val cos = (scala.math.sin(phi1) * scala.math.sin(phi2) * scala.math.cos(theta1 - theta2) + 
				scala.math.cos(phi1) * scala.math.cos(phi2))
		math.acos( cos ) * 3960.0
		
	}

    def Twits (cnt: Int) = {
      var qry = this.City + " " + this.State
      var sx1 = ""
      var pcnt = 0
      def formattwit (sx: String) = { 
        if (pcnt < cnt && !sx.contains("Hiring") && !sx1.contains(sx)) 
        {
    	  pcnt = pcnt + 1
	      sx1 += sx
	    	
	  	}
        sx
      }
      val twits = XML.load("http://search.twitter.com/search.atom?&q=" + qry) \\ "feed" \\ "title"   map(x => x.text.replaceAll("[\\r\\n]", ""))
      twits.take(cnt)
    }


}

object LocDB {

//	val LocaleDB: List[USLocale] = Source fromFile("/Users/walter_smith/work/uszipdemographRanking.csv") getLines()  map (l => new USLocale(l)) toList  
	val LocaleDB: List[USLocale] = Source fromURL("http://wssapp77.appspot.com/multiflex37/uszips.txt") getLines()  map (l => new USLocale(l)) toList  
  
	def loadLocales: List[USLocale] = LocaleDB

}

object ZipLookup extends App {

	def lookupOn (zip: String) = {


	  val Locales = LocDB.loadLocales
  
	  // Location and Profile
	  var a = Locales.find(_.Zip == zip).get
	  val Prof = new Profile("Hipster,1,0,2,0")
	  
	  // Sample Generic Selections
	  val within10 = Locales.filter(x => x.MilesFrom(a.Lat,a.Lng) < 10.0 )   // Within 10 Miles of Zip
	  val nearest10 = Locales.sortWith(_.MilesFrom(a.Lat,a.Lng) < _.MilesFrom(a.Lat,a.Lng)).take(11).tail //Closest 10 Zips excluding itself 
	  
	  nearest10.foreach(x => println(x.Zip,x.City,x.State))
	  
	  
	  // Highest Income Rank with n Miles of zip
	  val richest = within10.sortWith(_.IncomeRank > _.IncomeRank).take(1)(0)	  
	  println("Richest:" + richest)
	  

	  //val hippest = within10.sortWith(_.ProfileScore(Prof) > _.ProfileScore(Prof)).take(1)(0)
	  
	  // in One Line!
	  val hippy = Locales.filter(x => x.MilesFrom(a.Lat,a.Lng) < 10.0 ).sortWith(_.ProfileScore(Prof) > _.ProfileScore(Prof)).head
	  
	  
	  println("Hippest" + hippy)
	  val htwits = hippy.Twits(6).map(x => println(x))

	}

	Iterator.continually(Console.readLine).takeWhile(_ != "").foreach(line => lookupOn(line)) 

  
}

