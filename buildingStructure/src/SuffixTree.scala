
/**
 * @author jen
 */
import scala.collection.immutable.ListMap
object resultset{
  var result=  new StringBuilder()
  var resonnum = 0
  var Rules = 0
  def resetresonnum(num:Int)={
    resonnum = num
    
  }
  def addruleNum()={
    Rules +=1
    
  }
  def cleanruleNum()={
    Rules =0
    
  }
  var sortMap = scala.collection.mutable.Map[String,Double]()
  def Sorting() = {
         var result = ListMap(sortMap.toSeq.sortWith(_._2>_._2):_*)
    for(pattern <-result.keySet)
    {
      println("pattern:"+pattern)
      resultset.result.append(pattern)
    }
   // sortMap.clear()
  }
  
}


class SuffixTree {
    var RootTap = new StringBuilder()
    var rootnode = new Node(new StringBuilder())
    
  
    def CreatTree(elements:List[String]):SuffixTree={
          if(RootTap.isEmpty){
              RootTap.append(elements(0))          
            }
              var currentnode = rootnode   
          for(i<- 0 to elements.length-1)
          {
              val element = elements(i)
            
               if( i== elements.length-1){
              
                           if(currentnode.havethechild(element)){
                                    
                              currentnode = currentnode.getnext(element)
                              currentnode.addnumber()
                                        }
                                   
                          else{
                                     
                               val tempnode = new Node(new StringBuilder(element))
                               tempnode.addnumber()
                               currentnode.addnode(element, tempnode)
                                  }
                  
            }
               else if (i==0){
                           if(currentnode.havethechild(element)){
                              currentnode = currentnode.getnext(element)
                              currentnode.addnumber()
                             
                   }
                           else{
                                val tempnode = new Node(new StringBuilder(element))
                                tempnode.addnumber()
                                currentnode.addnode(element, tempnode)
                                currentnode = currentnode.getnext(element)
               }        
            }  
                else{
                           if(currentnode.havethechild(element)){
                             currentnode = currentnode.getnext(element)
                             currentnode.addnumber()                   
                     }
                           else{
                             val tempnode = new Node(new StringBuilder(element))
                             tempnode.addnumber()
                             currentnode.addnode(element, tempnode)
                             currentnode = tempnode
               }   
            }
            
        }
       return this     
      }
     def getTreeCode():String = {
       return new StringBuilder().append(RootTap).append(":(").append(rootnode.getnext(RootTap.result()).getcode()).append(")").result() 
     } 
      
     def  RebuildTree( treecode : String ): Unit={
       val treeroot = treecode.split(":")
       if(RootTap.length==0){
              RootTap.append(treeroot(0))          
            }
       
       RebuildNodes(treeroot(1), rootnode , 1 , treeroot(1).length()-1)      
      
     }         
     def RebuildNodes (treedata : String , IndexNode: Node , Low :Int, High:Int  ): Unit ={
          
       var tempindex = Low
       var nowindex = Low
       var nowchar  ='\0'
       var tempelement = ""
       var number = 0
          while(nowindex < High){
                 
              while(nowchar != '['){
                 
                  nowindex +=1
                  nowchar = treedata.charAt(nowindex)                               
              }
              tempelement = treedata.substring(tempindex, nowindex)
              tempindex = nowindex+1
              
              while(nowchar!= ']')
              {
                  nowindex +=1
                  nowchar = treedata.charAt(nowindex)
                
              }
               var tempnode = new Node(new StringBuilder(tempelement)) 
               number = treedata.substring(tempindex, nowindex).toInt
               tempnode.writenumber(number)
               tempnode.writename(tempelement) 
               tempnode.writelevel(checklevel(tempelement))                                     
               IndexNode.addnode(tempelement, tempnode)
               tempnode.writename(tempelement.split(";")(0)) 
            
               
           
               nowindex+=1
               tempindex =nowindex
               nowchar = treedata.charAt(nowindex)
                                                    
              
                if(nowchar =='('){
                    
                     nowindex +=1
                     nowchar = treedata.charAt(nowindex)
                              
                     var level =1       
                 while(level != 0)  // building tree based on this node     problem!!
                 {
                     
                     nowindex+=1
                     nowchar = treedata.charAt(nowindex)                 
                     if(nowchar == '('){
                   
                       level +=1
                     } 
                
                     if(nowchar == ')'){
                       level -=1
                 
                     }     
                    
                 }
                  RebuildNodes(treedata , tempnode , tempindex+1 , nowindex)
                  nowindex+=1
             
              } 
              tempindex = nowindex 
         } 
  
     }
       
        def Querry(text: String , support : Double , Level :Int):Unit= {
                  
          var temp = rootnode
          var pattern = new StringBuilder()
          var querry = text
          var inputnum = text.split("->").length-1
          
          //從根結點查詢
          if(inputnum ==0) {
             Querrytree(temp.getnext(text),support, pattern , temp.getnext(text),querry)
          
          }
          else{                 //由查詢最後的字處進入   
            
            
                for(i <-0 to inputnum) {
                           
                      if(i == text.split("->").length-1){
                        
                            println("entrance number is :"+ temp.getnext(text.split("->")(i)).numbers )
                           Querrytree(temp.getnext(text.split("->")(i)),support, pattern , temp.getnext(text.split("->")(i)),querry.split("->")(0))           
                       
                      }
                      else{                           
                             temp = temp.getnext(text.split("->")(i))                       
                             pattern.append(temp.namenode).append("->")                                              
                 }                 
             }         
          }      
      }
        //headnode 用途為參考進入點的support
        def Querrytree( Indexnode:Node , treshold:Double , pattern1 : StringBuilder , headnode : Node  , rootTap: String):Unit = {
          var tresh = treshold
          var pattern = pattern1
          var sonpattern = new StringBuilder()
          var parents = Indexnode
          var childnode = Indexnode
          var head = headnode
          var roottap = rootTap
          sonpattern.append(pattern)
        
          if(parents.havechildren()==false) {   
                if(childnode.namenode.split("_")(1).equals("q")|| childnode.namenode.split("_")(1).contains("Club")){
                sonpattern.append(childnode.namenode)
                
         
                var correct = ((childnode.numbers.toDouble / head.numbers.toDouble)*100).formatted("%.2f").toDouble
             
                println("head num:"+ head.numbers)
                resultset.resetresonnum(head.numbers)
                resultset.addruleNum()
                
                var popularity =  ((childnode.numbers.toDouble/3500)*100).formatted("%.2f").toDouble
                              sonpattern.append(" ").append(correct).append("% ").append(popularity).append("%").append(" ").append(childnode.numbers).append(":")
            
                 resultset.sortMap.put(sonpattern.result(),correct)
                 
                }
          }
    
          else{
           
          
              for(childkey <- parents.childnodes.keySet){  
                
                     childnode = parents.getnext(childkey)
                     
                    if(childnode.numbers.toDouble/head.numbers.toDouble >=tresh){
                                var p1 = new StringBuilder()                                   //
                                p1.append(parents.namenode).append("-")                          
                                sonpattern.append(parents.namenode).append("-")
                                Querrytree(childnode,tresh,sonpattern, head, roottap)                             
                                sonpattern.setLength(sonpattern.length-p1.length)       //刪除額外的字串(父親重複計算)
                }                          
             }
          }
       }
         
       def LevelSel(level:Int): Unit ={
             
         var indexnode = rootnode
         Restruct(indexnode , level)                  
        }
        //重新命名key的值
     def Restruct(indexnode : Node , level : Int) : Unit ={
           val children = indexnode.childnodes.keySet
           println(children)
      
             val le = level
             if(le ==1){ // 將namenode 裡的值轉換成KEY
               
                   if(indexnode.havechildren()){
               for(i<- children)
               {        
                      val newkey = indexnode.getnext(i).namenode              
                      val tempnode = indexnode.getnext(i)
                      indexnode.childnodes.remove(i)
                      indexnode.addnode(newkey, tempnode)
                    
                      val newnode = indexnode.getnext(newkey)
                      Restruct(newnode ,1)
               }
        
                   }
                   else{println("last")}
             }  
             else if (le ==2){
               
                     val indexnode = rootnode.getnext(RootTap.result())
                     val leveltext = indexnode.level2
                     indexnode.resetTag(leveltext)
                     indexnode.rewritenamenode(leveltext)
                     rootnode.childnodes.put(leveltext, rootnode.childnodes.remove(RootTap.result()).get)              
                     LevelTrans(rootnode , indexnode)
               
             }
             else{
             //  println("retype Level")
             }         
     }
     def checklevel(text :String) :String = {
       var LevelEle :String = ""
       
       if(text.split(";").length >1){
           LevelEle =  text.split(";")(1)
           return LevelEle         
        }
       else{
    
           return text.split(";")(0)
        } 
      }
        
     def Integrate(node1 : Node , node2 : Node): Node ={   //合併NODES
       if(node1 == node2){  
        // println("same")//記憶體位置相同是同一樣的物件
             return node1
       }
       val children1 = node1.childnodes
       val children2 = node2.childnodes
     
       val numbers1 = node1.numbers
       val numbers2 = node2.numbers
    
       for(key <- children2.keySet){    // 檢查下一層NODE是否有相同的level值
        
           if(children1.contains(key)){
             children1.put(key, Integrate(node1.getnext(key),node2.getnext(key)))
           }
           else{
             children1.put(key, node2.getnext(key))
             
           }
       }
       
       node1.numbers = numbers1+numbers2
          return node1
       }
     
     
     def LevelTrans( parent:Node , indexnode : Node){
       
           val indexlevel = indexnode.level2
           var children = indexnode.childnodes.keySet
       
           if(indexnode.havechildren()){
        
             indexnode.resetTag(indexlevel)
             indexnode.rewritenamenode(indexlevel)
         
             for(key <- children){   //INDEX 底下做轉換
            
                 val nownode = indexnode.getnext(key)
                 val nownodelevel = nownode.level2
                 val nownodenum = nownode.numbers
                 nownode.resetTag(nownodelevel)
                 nownode.rewritenamenode(nownodelevel)
                 indexnode.childnodes.remove(key)
             
              if(indexlevel == nownodelevel){     //上下相等 
                 
                  var tempnode = parent.getnext(indexlevel)
                   
                  parent.addnode(indexlevel, Integrate(nownode,indexnode))  //override if same key                                            
                  parent.getnext(indexlevel).numbers-=nownodenum   //扣掉重複計算的部分                
                  LevelTrans(parent , nownode)
                       
                 } 
             
              else  {       //左右相同
                          
                               if(indexnode.havethechild(nownodelevel)){
          
                                 indexnode.addnode(nownodelevel , Integrate(indexnode.getnext(nownodelevel),nownode)) 
                                           println("左右合聘:" + indexnode.getnext(nownodelevel).namenode+":"+indexnode.getnext(nownodelevel).numbers)
                                       
                                           }
                               else{
                                 
                                  indexnode.addnode(nownodelevel, nownode)
                                     } 
                      //  }
                }    
       
           }
           children = indexnode.childnodes.keySet
      
          
           for(key <- children){
                     LevelTrans(indexnode , indexnode.getnext(key))
        }              
      }  
       else{
        
         }           
    }
     
     def MergeRoot(): Unit = {
       if(rootnode.childnodes.size!=1){
         val node1 = rootnode.getnext((RootTap.result()))
         for(key <- rootnode.childnodes.keySet)
         {
           if(!node1.getTag().equals(key)){
             Integrate(node1 , rootnode.getnext(key))
           }
         }
         
       }
       else{
          
         //do nothing
       }
     }
  
   }
