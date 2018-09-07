object TranStd {
    // var tempstd ="0"
  def transaction(std:Double, ststd:Double ):String =
  {
  //  val ststd =(score-avg).formatted("%.2f").toDouble
    var tempstd ="0"
    
    ststd match {     
      /*
      case  ststd if(ststd>=2*(std)) => tempstd = "3"
      case  ststd if(ststd<2*(std) && ststd>=(std)) =>tempstd = "2"
      case  ststd if(ststd<(std) && ststd>=0) =>tempstd = "1"
      case  ststd if(ststd>(std)*(-1) && ststd<=0) =>tempstd = "*1"
      case  ststd if(ststd<=(std)*(-1) && ststd>std*(-2)) =>tempstd = "*2"
      case  ststd if(ststd<=(std)*(-2) )=>tempstd = "*3" 
      case _  => tempstd="null"
      
      */
      case  ststd if(ststd>=(std)) => tempstd = "2"
      case  ststd if(ststd<(std) && ststd>=0) =>tempstd = "1"
      case  ststd if(ststd< 0 && ststd>=(-1)*(std)) =>tempstd = "-1"
      case  ststd if(ststd<(-1)*(std) ) =>tempstd = "-2"
      
    }
    
   return tempstd     
  } 
}