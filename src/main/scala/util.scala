
package ensemble.math

import scala.math.pow

object mathUtil {

  def sumR(first : Double, ratio : Double, n : Int ) : Double = {
    ( first * ( pow(ratio,n) - 1 ) ) / ( ratio - 1 )
  }


}





