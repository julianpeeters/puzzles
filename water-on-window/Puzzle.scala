
object Puzzle extends App {

  val w = {
    List(//0,1,2,3,4,5,6,7,8,9
      List(0,1,0,0,0,0,1,0,0,0), //0
      List(0,1,1,0,0,0,0,1,1,0), //1
      List(0,1,1,0,0,0,0,1,0,0), //2
      List(0,1,0,0,0,0,0,0,0,0), //3
      List(0,0,0,0,0,0,0,0,1,0), //4
      List(0,0,0,1,1,0,0,0,1,0), //5
      List(0,0,0,1,1,0,0,0,1,0), //6
      List(0,0,0,0,0,1,1,0,1,0), //7
      List(0,0,0,0,0,1,1,0,1,0), //8
      List(0,0,0,0,0,0,0,0,0,0)) //9
    }

  val ones = for{
    (row, y)   <- w.zipWithIndex
    (value, x) <- row.zipWithIndex
    if (value == 1)        
  } yield (x,y)

  val adjacencyMap = ones.flatMap(coord => ones.collect{
    case (x, y) if {
      (x == coord._1 + 1 || x == coord._1 - 1) && y == coord._2 ^ 
      (y == coord._2 + 1 || y == coord._2 - 1) && x == coord._1
    }  => coord->(x,y)
  }).groupBy(_._1).map(n => (n._1->n._2.map(t=>t._2) ))

  val blobs = adjacencyMap.values.flatMap(v => v.map(coord => adjacencyMap(coord)++v))

  val largestBlob = blobs.collect{case x if (x.length == blobs.map(b=> b.length).max) => x}.head

  println(s"Size: ${largestBlob.length} Coordinates: $largestBlob")

val o=for{(r, y)<-w.zipWithIndex;(v,x)<-r.zipWithIndex;if(v == 1)}yield(x,y)
val a=o.flatMap(c=>o.collect{case(x,y)if{(x==c._1+1||x==c._1-1)&&y==c._2^(y==c._2+1||y==c._2-1)&&x==c._1}=>c->(x,y)}).groupBy(_._1).map(n => (n._1->n._2.map(t=>t._2)))
val b=a.values.flatMap(v=>v.map(c=>a(c)++v))
val l=b.collect{case x if (x.length==b.map(_.length).max)=>x}.head
println(s"Size: ${l.length} Coordinates: $l")


}




