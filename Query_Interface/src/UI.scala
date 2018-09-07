import scala.swing._
import swing.ComboBox
import scala.swing._
import scala.swing.event._
import scala.collection.mutable.ListBuffer
class UI extends MainFrame {

      val spark = new SparkConnect
      var totalRule = 0
   
      title = "Query "  
      var levelV =0
      var support =0.0
      var searchmode =0
      var buffitem = ""
      val treshold = new TextField(5) 
    
      val myListBuffer = ListBuffer("Paris", "New York", "Tokyo", "Berlin", "Copenhagen")
      val status1 = new RadioButton("level 1 ") {font = new Font("Arial", 0, 20)}
      val status2 = new RadioButton("level 2 ") {font = new Font("Arial", 0, 20)}
    
      val statusGroup = new ButtonGroup(status1, status2)
      var sit = new ListBuffer[String]()
      sit+="Reason----Numbers----Rules"
     
   preferredSize = new Dimension(1000, 500)
  def restrictHeight(s: Component) {
    s.maximumSize = new Dimension(Short.MaxValue, s.preferredSize.height)
  }
  def restrictHeight2(s: Component) {
    s.maximumSize = new Dimension(100, s.preferredSize.height)
  }
  val nameField = new TextField { 
     columns = 10
     font = new Font("Arial", 0, 20)
    }
  val commentField = new TextArea { 
    
    rows = 30
    wordWrap = true  
    font = new Font("Arial", 0, 20) 
     //Bounds(10, 30, 144, 18);
  }
  
 
  val FactorInfo = new ComboBox(sit)
  val myListView = new ListView(sit)
  
  
  val scroll = new ScrollPane(commentField)
  scroll.preferredSize = new Dimension(800,600)
  
  val Start = new ToggleButton("Start")
  val Clean = new ToggleButton("Clean")
  val Add = new ToggleButton("Add")
  val Remove = new ToggleButton("Remove")
  
  restrictHeight(nameField)
  restrictHeight(FactorInfo)
  restrictHeight(treshold)
  ///////////////////////////////////////// 版面排版
  contents = new BoxPanel(Orientation.Horizontal) {
    contents += new BoxPanel(Orientation.Vertical) {
    
               contents += new BoxPanel(Orientation.Horizontal) {
                   contents += new Label("Search:") {font = new Font("Arial", 0, 20)}
                   contents += Swing.HStrut(5)
                   contents += nameField
                 
         
  
    }    
               contents += new BoxPanel(Orientation.Horizontal) {
                    contents += new Label("Treshold:") {font = new Font("Arial", 0, 20)}
                  
                    contents += treshold     
                     
    }
      
   
    contents += new BoxPanel(Orientation.Horizontal) {
      
              contents += new Label("Numbers:") {font = new Font("Arial", 0, 20)}  // 字體樣式
              contents += Swing.HStrut(15)
              contents += FactorInfo
              
               //contents += myListView    
    }
   
     contents += new BoxPanel(Orientation.Horizontal){
      
        
              contents +=Swing.HGlue
          
           }
    contents += new BoxPanel(Orientation.Horizontal) {
              contents += status1
              contents += Swing.HStrut(10)
              contents += status2
              contents += Swing.HStrut(10)
            
    }
    contents += Swing.VStrut(5)
 
    contents += new BoxPanel(Orientation.Horizontal) { 
        
              contents += Start
              contents += Swing.HGlue
              contents += Remove
              contents += Swing.HGlue
              contents += Clean
              //contents += Button("Close") { reportAndClose() }
    }
    for (e <- contents)         // 排版
      e.xLayoutAlignment = 0.0
      border = Swing.EmptyBorder(10, 10, 10, 10)
     }
   contents += Swing.HStrut(50)
   contents += scroll
   
    for (e <- contents)         // 排版
      e.xLayoutAlignment = 0.0
     border = Swing.EmptyBorder(10, 10, 10, 10)
    
  }
    
 /////////////////////////////////////////  
  listenTo(nameField)
  listenTo(treshold)
  listenTo(commentField)
  listenTo(FactorInfo.selection)
 
  listenTo(status1)
  listenTo(status2)

  listenTo(Start)
  listenTo(Add)
  listenTo(Remove)
  listenTo(Clean)

 /////////////////////////////////////////  
  reactions += {
    case EditDone(`nameField`) => {
      println("search pattern : " + nameField.text)
     //sit::=nameField.text  
    }
    
     case EditDone(`treshold`) => {
       support = treshold.text.toDouble
      println("Your treshold is now: " + support) 

    }
    case EditDone(`commentField`) => 
      println("You changed the comments")
    case SelectionChanged(`FactorInfo`) =>{
      
   
      println("Your search is now: " + FactorInfo.selection.item)
      buffitem =FactorInfo.selection.item
      searchmode =0                      //暫存模式搜尋
      println("BUffer is "+buffitem)
    } 
  
   
       case ButtonClicked(`Start`) =>{
         
    
          
            ShowResult()
            if(!FactorInfo.item.contains(nameField.text +"--"+ resultset.resonnum +"--"+resultset.Rules))
            { 
              sit += nameField.text +"--"+ resultset.resonnum +"--"+resultset.Rules
              println("nope!")
         
            }
            
            println(nameField.text +":"+treshold.text)
            FactorInfo.repaint()
            resultset.cleanruleNum()
      }
       case ButtonClicked(`Clean`)=>{
          resultset.sortMap.clear()
           commentField.text= ""
       }
       
       case ButtonClicked(`Add`)  =>  {
            sit += nameField.text +":"+ resultset.resonnum+":"+totalRule
            println(nameField.text +":"+treshold.text)
          FactorInfo.repaint()
             Add.selected = false
             Add.repaint()
        }
       case ButtonClicked(`Remove`)  =>  {
         
          var removeitem = FactorInfo.selection.item
          sit.remove(sit.indexOf(FactorInfo.selection.item))
          
          FactorInfo.repaint()
           Remove.selected = false
           Remove.repaint()
        
        }
      case ButtonClicked(s) =>  {
         if(status1.selected)
         {
           levelV=1
            println("level 1 is selected: '" + levelV + "'")
         }
         else if(status2.selected)
         {
            levelV=2
            println("level 2 is selected: '" + levelV + "'")
         }
      } 
        
    
  }
///////////////////////////////////////////////////////   
  def ShowResult(){
    var t1 = System.nanoTime()
       
    if(searchmode==0){        //由搜尋取得
   
     spark.Search(nameField.text, support, levelV)
     }
    else if(searchmode ==1)    //由暫存取得
    {
      var item =FactorInfo.selection.item.split(":")(0)     // data from list
      levelV = FactorInfo.selection.item.split(":")(2).toInt
      support = FactorInfo.selection.item.split(":")(1).toDouble
      spark.Search(item, support, levelV)
      
    }
    
   var size= resultset.result.result().split(":").length
   totalRule = size
   var pattern = new StringBuilder()
   
   for(i<-0 to size-1){
        pattern.append(resultset.result.result().split(":")(i)).append("\n")
    }
    
      commentField.text = pattern.result()
      pattern.clear()
      resultset.result.clear()
      searchmode = 0
    //  nameField.text=""     // 執行完刪除搜尋字串
    // 刪除參考物件
     var t2 = System.nanoTime()
     println("Query Time:"+ (t2-t1)+"ns")
  } 
   val resultField = new TextArea { 
    rows = 10
    lineWrap = true
    wordWrap = true
    editable = false
  }

}