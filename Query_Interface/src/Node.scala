import scala.collection.mutable.Map
import java.io.Serializable
//import org.apache.spark.SparkContext._

/**
 * @author jen
 */
class Node(text:StringBuilder) extends Serializable{
      var level2 :String =""
      var childnodes : Map[String , Node]=Map() 
      var numbers :Int = 0
      var namenode : String = ""
      def cleannumber():Unit={
        numbers=0
      }
      def getTag():String = {
        return text.result()
        
      }
      def resetTag(te:String):Unit={
        text.clear()
        text.append(te)
       
      }
      def writelevel(text:String)
      {
        level2 = text
      }
      def writenumber(num :Int){
        numbers = num
      }
       
      def rewritenamenode(text:String){
        namenode= ""
        namenode = text
      }
      
      def writename(text:String)
      {
        namenode = text
        
      }
      def getname():String ={
        return namenode
      }
   
      def addnode(text:String , newnode :Node){
        childnodes.put(text, newnode) 
      }
      // true if there were not empty
      def havechildren():Boolean ={
        return !childnodes.isEmpty
             
      }
      // there has the specific child within the children
      def havethechild(text:String):Boolean={
        
          if(childnodes.isEmpty) return false 
          else if(childnodes.contains(text)) return true
          else return false       
      }
     
      def addnumber(){  
        
          numbers =numbers+1     
        }
      
      def getnum():Int={        
          return numbers     
      }
      def getnext(text:String):Node={
        
          return childnodes.get(text).get     
      }  
      
      
      def getcode(): StringBuilder = {
          if(this.childnodes.isEmpty){
              return new StringBuilder().append(text).append("[").append(numbers).append("]")             
          }
          else{ 
            
              var temp = new StringBuilder().append(text).append("[").append(numbers).append("]").append("(")
              for(key<- childnodes.keySet) {
                  temp.append(childnodes.get(key).get.getcode())
                }  
              temp.append(")")
              return temp
            }           
        }
}