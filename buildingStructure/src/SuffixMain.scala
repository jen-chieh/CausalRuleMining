import org.apache.log4j.Logger
import org.apache.log4j.Level
import org.apache.spark.{ SparkConf, SparkContext }
import org.apache.spark.rdd.RDD._
import org.apache.spark.storage.StorageLevel
import collection.mutable.HashMap
import scala.collection.Set
import scala.collection.mutable.StringBuilder
import org.apache.spark.SparkContext._
import org.apache.spark.SparkContext
import scala.util.Random
import java.util.Scanner
import org.apache.spark.{ SparkConf, SparkContext }
/**
 * @author jen
 */
object SuffixMain {  
    def main(args: Array[String]):Unit = {
      val t1 = System.nanoTime()
      Logger.getLogger("org").setLevel(Level.OFF)
                System.setProperty("spark.ui.showConsoleProgress", "false")

                println("開始執行RunSuffixTree")
                val sc = new SparkContext(new SparkConf().setAppName("STapp"))

                println("開始讀取文字檔...")

         
        print("execuation")
     // val sc = new SparkContext(new SparkConf().setAppName("testing").setMaster("local[4]"))
        println("getting data")
      val scorelist = sc.textFile(args(0)).repartition(args(1).toInt).persist()
      val zero = sc.broadcast(collection.mutable.Set[(String, String)]())
      var qData=scala.collection.mutable.Map[String,String] ()
      val listmap = scorelist.flatMap { line =>{
        
               val data = line.split(",")         
               val  classopen =data.apply(0)+"_"+data.apply(1)
               val  scoretypes = data.apply(2)+"_"+data.apply(3)          
                   List(classopen -> scoretypes)              
                
    } }.collectAsMap()    // turn all lectures of scores to a map for ref      
 
    val scorebro= sc.broadcast(listmap)
    val scan = new Scanner(System.in)
    val theme = sc.textFile(args(2)).repartition(args(3).toInt)
           
    val allSuffix = theme.flatMap(line => { 
           
         val tempString = StringBuilder.newBuilder
           
         if(line.split("/").length<3){               //dont need to convert score
             val last = line.split(",").length-1
             var temp = List[String]()
                   
              for (i <- 0 to last) yield {
                     temp = line.split(",").toList.drop(i)
                        (temp(0), temp)
              }      
             }
          else {                                      // need to convert scores
               var basicinfo = line.split("/")(0)
               var majorscore= line.split("/")(1)
               var gragu = line.split("/")(2).split(",")(1)                             
               tempString.append(basicinfo)
                for(i <- 0 to majorscore.split(",").length-1){
                                  
                  var major=  majorscore.split(",")(i).split(";")(0).split("_")(0)
                  var Clss = majorscore.split(",")(i).split(";")(0).split("_")(1)
                  var score = majorscore.split(",")(i).split(";")(0).split("_")(2)
                  var semester = majorscore.split(",")(i).split(";")(0).split("_")(3)
                  var year = majorscore.split(",")(i).split(";")(1)
                  var lesson =  major+"_"+Clss
                  tempString.append(lesson).append("_") 
                  
                       if(scorebro.value.contains(lesson)){  
                            val avg = scorebro.value(lesson).split("_")(0).toDouble
                            val std = scorebro.value(lesson).split("_")(1).toDouble
                            val tempscore =score.toDouble
                            val result = tempscore-avg
                            val stdrange =  TranStd.transaction(std, result)
                            tempString.append(stdrange).append("_").append(semester).append(";").append(year).append("_").append(stdrange).append(",")
                                 
                               }
                 
                        else{
                             tempString.append("NULL").append(" ")    
                               }      
                                           
                        
                           }
               
                         tempString.append(gragu)
            
                         val last =  tempString.result().split(",").length-1
                         var temp = List[String]()
                            for (i <- 0 to last) yield {
                                temp = tempString.result().split(",").toList.drop(i)
                                        (temp(0), temp)
                        }      
                      }
              
                })
                 
     val suffixReduce = allSuffix.groupByKey().mapValues(_.toList)
     val Tree = suffixReduce.map({ suffixset =>
                            //call Suffix Tree 
                         val suft = new SuffixTree()
                        for(suffix<-suffixset._2){
                               suft.CreatTree(suffix)                              
                        }
                     suft.getTreeCode()  
                })    
                
      val result = allSuffix.foreach { println }
    
      val Result = Tree.saveAsTextFile(args(4))
      val t2=   System.nanoTime()
         println("tota; excuttion time is :"+ (t2-t1)+" ns")
   sc.stop()
    
    }
}