package ensemble.tree

import ensemble.math.{ mathUtil => mu }


import scala.collection.mutable.ArrayBuffer
import scala.math.{log,sqrt,pow}

class BaseNode[+T](val data : Option[T], 
  var parent : BaseNode[Any],
  var child : BaseNode[Any],
  val id : Int) {

  var empty = false
  
  def this() {
    this(null,null,null,-1)
    this.empty = true
  }

  def this(data : T) {
    this(Option(data),null,null,-1)
  }

  def this(id : Int) {
    this(null,null,null,id) 
    this.empty = true
  }

  def pushBack[B >: T] (n:  B) {
      child = new BaseNode[B](n)
  }

}

class DummyNode[T]() extends BaseNode[T]() {


}

class UnaryNode[T] (val input : T,override val id : Int) extends BaseNode[T](input) {

  def this(data : T) {
    this(data,-1)
  }
  
}

class BinaryNode[T] (val input : T) extends BaseNode[T](input){

  var left : Option[BaseNode[T]] = Option(null)
  var right : Option[BaseNode[T]] = Option(null)

  def show() {
    println(data)
    println(left)
    println(right)
  }
}

class HeapTree[T](val comp : (T,T)=> Int) {

  var nodes : ArrayBuffer[T] = ArrayBuffer[T]()
  var empty : Boolean = true
  var nodeCount : Int = 0
  var depth : Int = 0

  var logf = (x : Double) => log(x) / log(2)

  def initRoot(data : T) {
    empty = false 
    nodes = ArrayBuffer[T](data,data)
    nodeCount = 2
  }
  
  def insert(data : T) {

    if ( nodeCount == 0 ) {
      initRoot(data)
      return
    }

    nodeCount += 1
    nodes ++= Seq(data)

    var parent = nodeCount / 2
    var cnodeCount = nodeCount - 1
    var p = true

    while( parent > 0 && p ) {

      println(nodes)
      var pdata = nodes(parent)
      var cdata = nodes(cnodeCount)
    
      if( comp(pdata,cdata) > 0 ) {
      
        var temp = pdata
        
        nodes(parent) = cdata
        nodes(cnodeCount) = temp
      }
      else {
        p = false
      }
      
      cnodeCount /= 2
      parent = cnodeCount / 2

    }
  }

  def show() {
    depth = logf(nodeCount.asInstanceOf[Double]).asInstanceOf[Int] + 1
    print(" lenth : ")
    println(nodeCount)
    print(" depth : ")
    println(depth)

    for( i <- 0 until depth ) {
      showline(i)
    }

  }

  def showline(n : Int) {
    val startPosition : Int = mu.sumR(1,2,n).asInstanceOf[Int] + 1
    val endPosition : Int = startPosition + pow(2,n).asInstanceOf[Int]
    var output : String = " " * ( ( pow(2, depth - n).asInstanceOf[Int] + n ) / 2 )

    //println(startPosition)
    //println(endPosition)

    var candidates = nodes.slice(startPosition,endPosition)

    print(output)

    candidates.foreach( nd => {
      print(nd)
      print(" " * (depth - n + 1) )
    })
      
    println(output)
  }

}

class Tree[T] {
  type HeapNode = BinaryNode[T];
  
  val dummy : DummyNode[T] = new DummyNode[T]()
  var root : HeapNode = null
  var empty : Boolean = true

  def initRoot(node: BinaryNode[T]) {
    this.root = node
  }

  def insert(node : HeapNode) {
    var index = 0;

    if (empty) {
      initRoot(node)
    }

    else {
      
    }
  }

}


