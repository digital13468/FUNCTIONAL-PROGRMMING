package patmat
// The base code was provided by Dr. Martin Odersky from his Coursera course, Functional Programming in Scala
import common._

/**
 * Huffman coding 
 *
 * Huffman coding is a compression algorithm that can be ed to compress lists of characters.
 * 
 * In a normal, uncompressed text, each character is represented by the same number of bits (usually eight). In Huffman coding, each 
 * character can have a bit representation of a different length, depending on how common a character is: the characters that appear often in
 * a text are represented by a shorter bit sequence than those being used more rarely. Every Huffman code defines the specific bit sequences
 * used to represent each character.
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   * 
   * Note that a given encoding is only optimal if the character frequencies in the encoded text match the weights in the code tree.
   * 
   * Every sub-tree is itself a valid code tree for a smller alphabet.
   * 
   * Encoding
   * 
   * For a given Huffman tree, one can obtain the encoded representation of a character by traversing from the root of the tree to the leaf
   * containing the character. Along the way, when a left branch is chosen, a 0 is added to the representaiton, and when a right brach is
   * chosen, 1 is added to the representation.
   * 
   * Decoding
   * 
   * Decoding also starts at the root of the tree. Given a sequence of bits to decode, we successively read the bits, and for each 0, we 
   * choose the left branch, and for each 1 we choose the right branch. When we reach a leaf, we decode the corresponding character and then
   * start again at the root of the tree.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree
  

  // using pattern matches on the code tree
  def weight(tree: CodeTree): Int =  // returns the total weight of a given Huffman tree  
    tree match {
      case Fork(left, right, chars, weight) => weight
      case Leaf(char, weight) => weight
  }
  
  def chars(tree: CodeTree): List[Char] =  // returns the list of chracters defined in a given Huffman tree 
    tree match {
      case Fork(left, right, chars, weight) => chars
      case Leaf(char, weight) => char :: Nil
  }
  // creation of Huffman trees by automatically calculating the list of characters and the weight
  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))



   
	// Generating Huffman trees

  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer.
   */
   def times(chars: List[Char]): List[(Char, Int)] = {
     def loop(remainingChars: List[Char], acc: List[(Char, Int)]) : List[(Char, Int)] = {
       if(remainingChars.isEmpty) acc
       else {
         val pair: (Char, Int) = (remainingChars.head, remainingChars.lastIndexOf(remainingChars.head) + 1)
         loop(remainingChars.slice(pair._2, remainingChars.length), acc ::: (pair :: Nil))
       }
     }
     loop(chars.sorted, Nil)
   }
  
  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
   def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
     def loop(sortedFreqs: List[(Char, Int)], acc: List[Leaf]): List[Leaf] = {
       if(sortedFreqs.isEmpty) acc else loop(sortedFreqs.tail, acc ::: (Leaf(sortedFreqs.head._1, sortedFreqs.head._2) :: Nil))
     }
     loop(freqs.sortBy(pair => pair._2), Nil)
   }
  
  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
   def singleton(trees: List[CodeTree]): Boolean = trees.length == 1
  
  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
   def combine(trees: List[CodeTree]): List[CodeTree] = {
     if(trees.length < 2) trees
     else {
       def loop(remainingList: List[CodeTree], newNode: CodeTree): List[CodeTree] = {
         if(remainingList.isEmpty) Nil.::(newNode)
         else if(remainingList.length == 1) {
           if(weight(remainingList.head) > weight(newNode)) (newNode :: Nil) ::: remainingList
           else remainingList ::: (newNode :: Nil)
         }
         else {
           if(weight(remainingList(remainingList.length / 2)) > weight(newNode))
             loop(remainingList.slice(0, remainingList.length / 2), newNode) ::: 
               remainingList.slice(remainingList.length / 2, remainingList.length)
           else if(weight(remainingList(remainingList.length / 2)) < weight(newNode)) 
             remainingList.slice(0, remainingList.length / 2) :::
               loop(remainingList.slice(remainingList.length / 2, remainingList.length), newNode) 
           else
              remainingList.slice(0, remainingList.length / 2) ::: (newNode :: Nil) ::: 
                remainingList.slice(remainingList.length / 2, remainingList.length)
         }
       }
       loop(trees.slice(2, trees.length), 
           Fork(trees(0), trees(1), chars(trees(0)) ::: chars(trees(1)), weight(trees(0)) + weight(trees(1))))
     }
   }
  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   */
   def until(singleton: List[CodeTree] => Boolean, combine: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] =
     if(singleton(trees)) trees else until(singleton, combine)(combine(trees))
  
  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
   def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine)(makeOrderedLeafList(times(chars)))(0)
  

  // Decoding

   type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
   def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
     def loop(remainingBits: List[Bit], subTree: CodeTree, acc: List[Char]): List[Char] = {
       subTree match {
         case Fork(left, right, chars, weight) => 
           if(remainingBits.head.==(0)) loop(remainingBits.tail, left, acc) else loop(remainingBits.tail, right, acc)
         case Leaf(char, weight) => if(remainingBits.isEmpty) acc ::: char :: Nil else loop(remainingBits, tree, acc ::: char :: Nil)
       }
     }
     loop(bits, tree, Nil)
   }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
   val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
   val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

  /**
   * returns the decoded secret
   */
   def decodedSecret: List[Char] = decode(frenchCode, secret)
  

  // Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
   def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
     def isLeft(subTree: CodeTree, charToCheck: Char): Boolean = {
       subTree match {
         case Fork(left, right, chars, weight) => chars.contains(charToCheck)
         case Leaf(char, weight) => char.==(charToCheck)
       }
     }
     def loop(subTree: CodeTree, remainingText: List[Char], acc: List[Bit]): List[Bit] = {
       if(remainingText.isEmpty) acc
       else 
         subTree match {
           case Fork(left, right, chars, weight) => 
             if(isLeft(left, remainingText.head)) loop(left, remainingText, acc ::: (0 :: Nil))
             else loop(right, remainingText, acc ::: (1 :: Nil))
           case Leaf(char, weight) => loop(tree, remainingText.tail, acc)
         }
     }
     loop(tree, text, Nil)
   }
  
  // Encoding using code table which encodes an equivalent representation but more efficiently
  

   type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
   def codeBits(table: CodeTable)(char: Char): List[Bit] = if(table.head._1.==(char)) table.head._2 else codeBits(table.tail)(char)
  
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   */
   def convert(tree: CodeTree): CodeTable = {
     tree match {
       case Fork(left, right, chars, weight) => mergeCodeTables(convert(left), convert(right))
       case Leaf(char, weight) => (char, Nil) :: Nil
     }
      // another way of implementation without using mergeCodeTables
      /*def loop(subTree: CodeTree, acc: CodeTable, code: List[Bit]): CodeTable = {
        subTree match {
          case Fork(left, right, chars, weight) => loop(right, loop(left, acc, code ::: 0 :: Nil), code ::: 1 :: Nil)
          case Leaf(char, weight) => acc ::: (char, code) :: Nil
        }
      }
      loop(tree, Nil, Nil)*/
   }
  
  /**
   * This function takes two code tables and merges them into one. 
   */
   def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
     def loop(remainingCodeTable: CodeTable, acc: CodeTable, prefix: Bit): CodeTable = {
       if(remainingCodeTable isEmpty) acc
       else loop(remainingCodeTable.tail, acc ::: (remainingCodeTable.head._1, prefix :: remainingCodeTable.head._2) :: Nil, prefix)
     }
     loop(a, Nil, 0) ::: loop(b, Nil, 1)
   }
  
  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
   def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
     def loop(codeTable: CodeTable, remainingText: List[Char]): List[Bit] = {
       if(remainingText.length == 1)
         codeBits(codeTable)(remainingText.head)
       else
         codeBits(codeTable)(remainingText.head) ::: loop(codeTable, remainingText.tail)
     }
     loop(convert(tree), text)
   }
    
    def main(args: Array[String]) {
      val sampleTree = makeCodeTree(makeCodeTree(Leaf('x', 1), Leaf('e',1)), Leaf('t', 2))
      println(weight(sampleTree))  // 4
      println(chars(sampleTree))  // List(x, e, t)
      val sampleString = string2Chars("the code table `table`.")  
      println(times(sampleString))  // List(( ,3), (.,1), (`,2), (a,2), (b,2), (c,1), (d,1), (e,4), (h,1), (l,2), (o,1), (t,3))
      
      val sampleList = List(('t', 2), ('e', 1), ('x', 3))
      println(makeOrderedLeafList(sampleList))  // List(Leaf('e',1), Leaf('t',2), Leaf('x',3))
      val leaflist = List(Leaf('c', 1), Leaf('d', 1), Leaf('e', 1), Leaf('f', 1), Leaf('g', 1), Leaf('h', 1), Leaf('b', 3), Leaf('a', 8))
      println(combine(leaflist))  // List(Leaf(e,1), Leaf(f,1), Leaf(g,1), Leaf(h,1), Fork(Leaf(c,1),Leaf(d,1),List(c, d),2), Leaf(b,3), Leaf(a,8))
      println(createCodeTree(sampleString))
      val sampleCodeTree = until(singleton, combine)(leaflist).head
      println(sampleCodeTree)
      println(encode(sampleCodeTree)(decode(sampleCodeTree, List(1, 1, 0, 1))))  // List(1, 1, 0, 1)
      println(encode(sampleCodeTree)(decode(sampleCodeTree, List(1, 1, 1, 0, 1, 1, 0, 0))))  // List(1, 1, 1, 0, 1, 1, 0, 0)
      println(convert(sampleCodeTree))  // List((a,List(0)), (e,List(1, 0, 0, 0)), (f,List(1, 0, 0, 1)), (g,List(1, 0, 1, 0)), (h,List(1, 0, 1, 1)), (c,List(1, 1, 0, 0)), (d,List(1, 1, 0, 1)), (b,List(1, 1, 1)))
      println(quickEncode(sampleCodeTree)(string2Chars("d")))  // List(1, 1, 0, 1)
      println(quickEncode(sampleCodeTree)(string2Chars("bac")))  // List(1, 1, 1, 0, 1, 1, 0, 0)
    }
  }
