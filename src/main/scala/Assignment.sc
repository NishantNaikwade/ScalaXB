import math.abs
object DemoExercise{
  def factorial(n: Int):Int = {
    if(n==0) 1 else n*factorial(n-1)
  }
  factorial(5)
  def factorialRec(n:Int):Int ={
    def loop(acc:Int,n:Int):Int={
      if(n==0)acc else loop(n*acc,n-1)
    }
    loop(1,n)
  }
  factorialRec(5)
  def sum(f:Int=>Int,a:Int,b:Int):Int ={
    def loop(a:Int,acc:Int):Int ={
      if(a>b) acc else loop(a+1,f(a)+acc)
    }
    loop(a,0)
  }
  sum(x => x * x,3,5)
  sum(x => x * x * x,3,5)
  sum(x=>factorial(x),1,3)
  def product(f:Int => Int )(a:Int,b:Int):Int={
    if(a>b) 1 else f(a) * product(f)(a+1,b)
  }
  product(x => x )(3,4)
  def factorialUsingProduct(n:Int):Int = {
    if(n==0) 1 else product(x=>x)(1,n)
  }
  factorialUsingProduct(5)
  val tolerance = 0.0001
  def isCloseEnough(x:Double,y:Double) = {
    println("Inside isCloseEnough : " + y)
    val closeValue = abs((x-y) / x) / x
    println("CloseValue = " + closeValue)
    closeValue < tolerance
  }
  def fixedPoint(f:Double=> Double)(firstGuess: Double) = {
    println("inside fixed point")

    def iterate(guess:Double):Double={
      println("Iterate")
      val next=f(guess)
      if(isCloseEnough(guess,next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }
  fixedPoint(x=> 1+x/2)(1)
  //def averageDamp(f:Double=>Double)(x:Double)= (x+f(x))/2
  def sqrt(x:Double) = fixedPoint(y => (y + x / y)/2)(1)

  ((2+1)/2) * ((2+1)/2)

  //((sqrValue+guess)/2) * ((sqrValue+guess)/2) > sqrValue
  sqrt(2)
}