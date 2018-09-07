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

/**
 * @author jen
 */
class SparkConnect {
      Logger.getLogger("org").setLevel(Level.OFF)
      System.setProperty("spark.ui.showConsoleProgress", "false")
     
      val sc = new SparkContext(new SparkConf().setAppName("testing").setMaster("local[4]"))
      // val treecode = sc.textFile(" source location")
      val treecode = sc.textFile("hdfs://master:9000/user/jen/data/Trees")
  

      def Search(Pattern:String , support: Double , level : Int) : Unit = {
    
         val queryresult = treecode.filter (line => line.split(":")(0).contains(Pattern.split("->")(0)))
        
         if(level ==1){
             val Result = queryresult.map ({ querycode =>  
                                              val tree=new SuffixTree()
                                              tree.RebuildTree(querycode)
                                              tree.LevelSel(level)
                                              tree.Querry(Pattern, support,level)
                                              resultset.Sorting()
                                              resultset.result.result()
                                           
                                                      })
          
           val recomm = Result.map { line =>  line.split(":") }.flatMap { x => x }.foreach { println }                               
         }
         
         else if(level==2){
           val patterncollect = queryresult.map{ line =>{
                  //var temp = List[String]()
                   var key = line.split(":")(0).split(";")(1)
                  if(line.split(":")(0).split(";").length==1 ){   
                    key = line.split(":")(0).split(";")(0)
                  }
                
                   var value = line
                           
                   if( Pattern.split("->")(0).split("_")(0).toInt.isValidInt && Pattern.split("->")(0).split("_")(0).toInt >99 ){
                      var  year =Pattern.split(",")(0).split("_")(0).toInt*10
                      var  semester =  line.split(":")(0).split(";")(0).split("_")(3).toInt
                    
                        if(semester-year==1) {
                           (key,value)
                        }
                        else{
                          
                          ("none","1")
                        }
                                }
                  
                  else{
                   
                   (key,value)
                  }
                }}
                        
                val result = patterncollect.foreach(println)       
                val  CleanData= patterncollect.filter(line => line._1 !="none").groupByKey().mapValues(_.toList)      //filter useless data
                     
                val Result = CleanData.map(treeset =>{
                     var suffixtree =  new SuffixTree()
                     
                     for(treecode<-treeset._2)yield{
                       
                          suffixtree.RebuildTree(treecode)                       
                     }
                     suffixtree.MergeRoot()
                     suffixtree.LevelSel(2)                  
                     suffixtree.Querry(Pattern, support,level)
                     
                       resultset.Sorting()
                       resultset.result.result()  
                      }
                
              )
              
               
             val recomm2 = Result.collect()       
         }
         
       
  }
  
  def StopSpark():Unit = {
    sc.stop()
    
  }
  
}