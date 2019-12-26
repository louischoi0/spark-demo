
import ensemble.tree.BinaryNode
import ensemble.tree.HeapTree
import ensemble.s._
import ensemble.s.{CSVDataSource,CSVExtractor}

import scala.collection.mutable._

object Main extends App {

  def testHeapTree() {
    println("Hello, World!")

    var h = new HeapTree[Int]((a:Int,b:Int) => a-b)

    var n = new BinaryNode[Int](2)

    h.insert(0)
    h.insert(9)
    h.insert(4)
    h.insert(10)
    h.insert(1)
    h.insert(3)
    h.insert(8)
    h.insert(2)
    h.insert(6)
    h.insert(7)
    h.insert(5)
    h.insert(3)
    h.insert(3)
    h.show()

  }

  def test() {
    var dx = new DataSource()
    dx.builder.op("a","b").op("c","d").op("file","a.csv")

    println(dx.optionSet)



  }

  def testFactory() {
    var fname = "/tmp/train.csv"

    var w = new WorkFlow()

    var ds =  new CSVDataSource(fname)

    var n1 = new CSVExtractor(ds)

    def k(a : ArrayBuffer[Any]) : ArrayBuffer[Any] =  {
      a(0) += "a"
      a
    }

    def k1(a : ArrayBuffer[Any]) : ArrayBuffer[Any] =  {
      a(0) += "b"
      a
    }

    var n2 = new TransFormFunction(k)
    var n3 = new TransFormFunction(k1)

    def a0(a : ArrayBuffer[Any]) : String =  {
      a(1).toString ++ a(4).toString
    }

    def a1(a : ArrayBuffer[Any]) : String =  {
      a(4).toString
    }

    var n4 = new Aggregation(a1)

    w.addNode(n1)
    w.addNode(n2) 
    w.addNode(n3) 
    w.addNode(n4)
   
    var result = w.proceed
    print(result)

  }

  testFactory
}









