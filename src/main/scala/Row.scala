package ensemble.s

import scala.collection.mutable.ArrayBuffer

class Schema {
  var fields : ArrayBuffer[String] = new ArrayBuffer[String]()
  var fieldTypes : ArrayBuffer[String] = new ArrayBuffer[String]()

  def addField(name : String, ftype : String) {
    fields ++ Seq(name) 
    fieldTypes ++ Seq(ftype)
  }


}


class Row(schema : Schema) {
  var data = ArrayBuffer[Double]()

  def getItem(i : Int) {
    data(i)
  }
  

}





