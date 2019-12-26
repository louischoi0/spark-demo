package ensemble.s

import ensemble.tree._

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import com.github.tototoshi.csv._

import java.io.File

class ActionNode( actionNode :Action )  extends BaseNode[Action](actionNode) {


}


abstract class Action {
  var prepared : Boolean = false
  def prepare(parent : Action) : Unit
  def next() : ArrayBuffer[Any]

}


class WorkFlow {
 
  var root : ActionNode = null
  var p : BaseNode[Any] = root

  def addNode(a : Action) {
    if (root == null) {
      root = new ActionNode(a)
      p = root
    }

    else {
      a.prepare(p.data.get.asInstanceOf[Action])
      p.pushBack(a)  
      p = p.child
    }
  }

  def proceed() : ArrayBuffer[AnyRef] = {
    var result : ArrayBuffer[AnyRef] = new ArrayBuffer[AnyRef]()

    var loopf = true

    while( loopf ) {
      
      try {
        var r : ArrayBuffer[Any] = p.data.get.asInstanceOf[Action].next
        result += r
      }

      catch {
        case e : java.util.NoSuchElementException => {
          loopf = false
        }
      } 
    }

    result
  }

}


class DataSource extends Action {

  var optionSet : Map[String,String] = Map[String,String]()
  var data : ArrayBuffer[Int] = ArrayBuffer[Int]()
  var p : Int = 0
  var workderNum : Int = 0

  
  override def prepare(parent : Action) : Unit = {

  }

  override def next() : ArrayBuffer[Any] = {
    new ArrayBuffer[Any]()
  }

  class _builder(s : DataSource) {

    def op(optionKey : String, value : String) : _builder = {
      s.optionSet += ( optionKey -> value )
      this
    }
  }

  val builder : _builder = new _builder(this)

}

class CSVDataSource(fname : String) extends DataSource {
  builder.op("format","csv")
  builder.op("file",fname)
}


abstract class Extractor(s : DataSource) extends Action {

  override def prepare(parent: Action): Unit
  override def next() : ArrayBuffer[Any]

}

class CSVExtractor(s : CSVDataSource) extends Extractor(s) {
  var reader : CSVReader = null 
  var data :ArrayBuffer[Any] = new ArrayBuffer[Any]()

  var iterator : Any = null

  override def prepare(parent: Action): Unit = {
    val filename : String = s.optionSet("file")
    reader = CSVReader.open(new File(filename))
    iterator = reader.iterator
    prepared = true
  }

  override def next() : ArrayBuffer[Any] = {
    if ( !prepared) {
      prepare(null)
    }

    var row = reader.iterator.next

    var rdata = new ArrayBuffer[Any]()
    rdata ++ row
  }
  
}

abstract class Transformer extends Action {
  def iterator() : Int

}

class TransFormFunction(val expr : (ArrayBuffer[Any]) => ArrayBuffer[Any]) extends Action {

  def this() {
    this(null) 
  }

  var it : Action = null

  def prepare(parent : Action) {
    it = parent
    prepared = true
  }

  def next() : ArrayBuffer[Any] = {
    expr(it.next)    
  }

}

class Aggregation( val groupingExpr : (ArrayBuffer[Any]) => String ) extends Action {

  var data : Map[String, ArrayBuffer[Any]] = Map[String, ArrayBuffer[Any]]()

  var it : Action = null

  def prepare(parent : Action) {
    it = parent
    prepared = true
  }

  override def next() : ArrayBuffer[Any] = {
    var r : ArrayBuffer[Any] = it.next
    var key : String = groupingExpr(r)

    data += ( key -> r )
    ArrayBuffer(data)
  }

  def excute() {
    

  }

}






