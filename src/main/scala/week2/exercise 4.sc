object exercise4 {
  def product(f: Int =>Int) (a:Int,b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f) (a+1,b)
  }
  def factorial(n: Int): Int = {
    if(n == 0) 1
    else product(x=> x)(1,n)
  }

  def mapReduce(f: Int => Int,combine: (Int,Int)=>Int,zero:Int)(a:Int,b: Int): Int = {
    if(a > b) zero
    else combine(f(a),mapReduce(f,combine,zero)(a+1,b))
  }

  def productMapReduce(f: Int =>Int) (a:Int,b: Int): Int = mapReduce(f,(x,y)=> x * y,1)(a,b)
  def sumMapReduce(f: Int =>Int) (a:Int,b: Int): Int = mapReduce(f,(x,y)=> x + y,0)(a,b)

  product(x=>x*x)(3,4)
  productMapReduce(x=>x*x)(3,4)
  sumMapReduce(x=>x+x)(3,4)
  factorial(3)

}