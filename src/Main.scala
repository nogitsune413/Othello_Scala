import scala.io._

object Main {

  val L       = 10
  val X       =  0
  val Y       =  1
  val EMPTY   =  0
  val BLACK   =  1
  val WHITE   =  2
  val DRAW    =  3
  val WALL    =  9
  val MOVE    =  1
  val PASS    =  2
  val GIVE_UP =  3
  val EXIT    =  4

  val dir     = Array(-1,0,1,1,1,0,-1,-1) zip Array(-1,-1,-1,0,1,1,1,0)
  var board   = Array.ofDim[Int](L,L)
  var x,y     = -1
  var victory = DRAW

  val LINE       = "------------------------------\n"
  val TITLE      = "\n" + LINE + "---      オセロ          ---\n" + LINE + "\n"
  val USAGE      = "  ---  遊び方   --- \n 縦 5,横 3 のマスに置く => 「5 3」と入力。\n パス： pass \n 投了：give up\n ゲームの終了： exit\n"
  val URGES      = "\n駒を置いて下さい  => "
  val RANGE      = "^[1-8]$"
  val S_BLACK    = "黒"
  val S_WHITE    = "白"
  val S_TURN     = "番"
  val S_DRAW     = "\n   ---    引き分け   ---  \n"
  val PRE_LINE   = "\n   ---  "
  val POST_LINE  = "      ---   \n\n"
  val BLACK_TURN = PRE_LINE + S_BLACK + S_TURN + POST_LINE
  val WHITE_TURN = PRE_LINE + S_WHITE + S_TURN + POST_LINE
  val S_VICTORY  = "の勝ち   ---   \n"
  val S_ERROR    = "駒が置けません。別のマスを選択して下さい。\n"
  val IN_ERROR   = "入力に誤りがあります。再度入力して下さい。\n"
  val THANKS     = "Thank you for playing. Good by the next time."

  object Turn{
    var thisTurn   = BLACK
    def show() = {
      if(thisTurn==BLACK){print(BLACK_TURN)}
      else               {print(WHITE_TURN)}
    }
    def shift() = {
      thisTurn = 3 - thisTurn
    }
    def otherside():Int = {
      3 - thisTurn
    }
  }

  object Board{
    def init() = {
      for(i <- 0 to L - 1 ){
        board(0)(i) = WALL
        board(9)(i) = WALL
        if(i != 0 && i != 9){
          for(i <- 1 to 8){
            board(i)(0) = WALL
            board(i)(9) = WALL
          }
        }
      }
      for(i <- 4 to 5){
        for(j <- 4 to 5){
          if(i==j){
            board(i)(j) = WHITE
          } else {
            board(i)(j) = BLACK
          }
        }
      }
    }
    def show() = {
      print(LINE)
      print("     ")
      for(i <- 1 to L - 2){
        print(i)
        if(i<L-2){print(" ")}
      }
      for(i <- 0 to L-1){
        if(0<i && i<L-1){
          print("\n " + i + " ")
        } else {
          print("\n   ")
        }
        for(j <- 0 to L-1){
          board(i)(j) match {
            case WALL  => print("+")
            case EMPTY => print("_")
            case WHITE => print("o")
            case BLACK => print("*")
          }
          print(" ")
        }
        if(i<L-1){print(" ")}
        else     {print("\n")}
      }
      print(LINE)
    }
  }

  def main(args:Array[String]) = {

    Board.init()

    println(TITLE)
    println(USAGE)

    UI
  }

  def UI:Unit = {
    Turn.show()
    Board.show()
    print(URGES)
    val cmd = input()
    cmd match {
      case MOVE    => if(update){
        if(isFill){
          victory = judge()
          show_result()
        } else {
          Turn.shift()
          UI
        }
      } else {
        println(S_ERROR)
        UI
      }
      case PASS    => Turn.shift()
        UI
      case GIVE_UP => victory = Turn.otherside
        show_result()
      case EXIT    => println(THANKS)
    }
  }

  def show_result() = {
    var result = ""
    victory match {
      case BLACK => result = PRE_LINE + S_BLACK + S_VICTORY
      case WHITE => result = PRE_LINE + S_WHITE + S_VICTORY
      case DRAW  => result = S_DRAW
    }
    println(result)
    Board.show()
  }

  def input():Int = {
    val in = readLine()
    in match {
      case "pass"    => PASS
      case "exit"    => EXIT
      case "give up" => GIVE_UP
      case _         => val ar = in.split("\\s")
        if(ar.length==2 && ar(0).matches(RANGE) && ar(1).matches(RANGE)){
          y = ar(0)toInt;
          x = ar(1)toInt;
          MOVE
        } else {
          println(IN_ERROR)
          println(USAGE)
          println(URGES)
          input()
        }
    }
  }

  def judge():Int = {
    if(count(BLACK) < count(WHITE)){
      WHITE
    } else if(count(BLACK)>count(WHITE)){
      BLACK
    } else {
      DRAW
    }
  }

  def isFill:Boolean = {
    if(count(BLACK)+count(WHITE)==8*8){
      true
    } else {
      false
    }
  }

  def count(color:Int):Int = {
    count(board,color)
  }

  def count(brd:Array[Array[Int]],color:Int):Int = {
    if(brd.isEmpty){
      0
    } else {
      (brd.head filter (_ == color)).length + count(brd.tail,color)
    }
  }

  def update() = {
    if(chk_square()){
      flip()
      true
    } else {
      false
    }
  }

  def chk_square():Boolean = {
    if(board(y)(x)!=EMPTY){return false}
    for(dr <- dir){
      if(board(y + dr._2)(x + dr._1)==Turn.otherside){
        if(chk_square_a(x + dr._1 * 2,y + dr._2 * 2,dr)){
          return true;
        }
      }
    }
    false
  }

  def chk_square_a(a:Int,b:Int,dr:(Int,Int)):Boolean = {
    board(b)(a) match {
      case n if(n==Turn.thisTurn)  => true
      case n if(n==Turn.otherside) => chk_square_a(a + dr._1,b + dr._2,dr)
      case _                       => false
    }
  }

  def flip(){
    board(y)(x) = Turn.thisTurn
    for(dr <- dir){
      if(board(y + dr._2)(x + dr._1)==Turn.otherside){
        flip_a(x + dr._1 * 2,y + dr._2 * 2,dr)
      }
    }
  }

  def flip_a(a:Int,b:Int,dr:(Int,Int)):Unit = {
    board(b)(a) match {
      case n if(n==Turn.thisTurn)  => flip_b(a - dr._1,b - dr._2,dr)
      case n if(n==Turn.otherside) => flip_a(a + dr._1,b + dr._2,dr)
      case _                       => ;
    }
  }

  def flip_b(a:Int,b:Int,dr:(Int,Int)):Unit = {
    if (board(b)(a)!=Turn.otherside){
      ;
    } else {
      board(b)(a) = Turn.thisTurn
      flip_b(a - dr._1,b - dr._2,dr)
    }
  }
}