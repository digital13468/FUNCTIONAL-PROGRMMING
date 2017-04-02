
object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
	
    print(balance("(if (zero? x) max (/ 1 x))".toList))
	
    print(countChange(4,List(1,2)))
  }

  /**
   * Pascal's Triangle
   * 
   * The following pattern of numbers is called Pascal's triangle
   * 
   *     1
   *	1 1
   *   1 2 1
   *  1 3 3 1
   * 1 4 6 4 1
   *    ...
   *    
   * The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers
   * above it. The function below computes the element of Pascal's triangle by means of a recursive process. It takes a
   * column c and a row r, counting from 0 and returns the number at that spots in the triangle. For example, 
   * pascal(0,2)=1, pascal(1,2)=2 and pascal(1,3)=3. 
   */
    def pascal(c: Int, r: Int): Int = {
      if (c > r)
        0
      else if (c == 0 || r == 0)
        1
      else
        pascal((if (c < 1) 0 else c - 1), (if (r < 1) 0 else r - 1)) + pascal(c, (if (r < 1) 0 else r - 1))     
    }
  
  /**
   * Parentheses Balancing
   * 
   * The following is a recursive function which verifies the balancing of parentheses in a string, which we represent as a
   * List[Char]. For example, the function returns true for the following strings:
   * 
   * 	(if (zero? x) max (/ 1 x))
   * 	I told him (that it’s not (yet) done). (But he wasn’t listening)
   * 
   * The function returns false for the following strings:
   * 
   * 	:-)
   * 	())(
   */
    def balance(chars: List[Char]): Boolean = {
      def unmatchedOpening(chars: List[Char]): Int = {
        if (chars.isEmpty)
          0        
        else {
          val unmatchedOpeningInTail = unmatchedOpening(chars.tail)
          if (chars.head.==('('))       
            if (unmatchedOpeningInTail > -1)
              Int.MaxValue
            else
              unmatchedOpeningInTail + 1
          else if (chars.head.==(')'))
            unmatchedOpeningInTail - 1
          else
            unmatchedOpeningInTail
        }
      }
      unmatchedOpening(chars).==(0)
    }
  
  /**
   * Counting Change
   * 
   * The next recursive function counts how many different ways you can make change for an amount, given a list of coin
   * denominations. For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 
   * 1+1+1+1, 1+1+2, 2+2. The function takes an amount to change, and a list of unique denominations for the coins.
   * 
   * For example, the function returns the following number, given the money and coins.
   * countChange(4,List(1,2)) = 3
   * countChange(300,List(5,10,20,50,100,200,500)) = 1022
   * countChange(301,List(5,10,20,50,100,200,500)) = 0
   * countChange(300,List(500,5,50,100,20,200,10)) = 1022
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (money > 0 && !coins.isEmpty)
        countChange((money - coins.head), coins) + countChange(money, coins.tail)
      else
        0
        
      // Another way of implementing the function, with a loop
      /*if (!coins.isEmpty) {
        var waysWithHead = 0;
        for ( coinsOfHead <- 0 to (money/coins.head).toInt) {
          if ((money - coins.head * coinsOfHead) != 0) {
            val waysWithoutHead = countChange(money - coins.head * coinsOfHead, coins.tail)
            waysWithHead = waysWithHead + waysWithoutHead
          }
          else
            waysWithHead = waysWithHead + 1;
        }
        waysWithHead
      }
      else
        0*/
    }
  }
