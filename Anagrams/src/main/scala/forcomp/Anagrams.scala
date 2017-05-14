package forcomp
// The base code was provided by Dr. Martin Odersky from his Coursera course, Functional Programming in Scala
/** An anagram of a word is a rearrangement of its letters such that a word with a different meaning is formed. For example, if we rearrange
 *  the letters of the word 'Elvis', we can obtain the word 'lives', which is one of its anagrams.
 *  
 *  In a similar way, an anagram of sentence is a rearrangement of all the chracters in the sentence such that a new sentence is formed. The 
 *  new sentence consists of meaningful words, the number of which may or may not correspond to the number of words in the original sentence.
 *  For example, the sentence "I love you" is an anagram of the sentence "You olive". We consider permutations of words anagrams of the
 *  sentence. In the above example, "You I love" is considered a separate anagram.
 *  
 *  When producing anagram, we ignore character casing and punctuation chracters.
 *  
 *  The method 'sentenceAnagrams', given a list of words representing a sentence, finds all the anagrams of that sentence. A dictionary, i.e.
 *  a list of words is given for indicating words that have a meaning.
 *  
 *  We transform the characters of the sentence into a list saying how often each character appears, called occurrence list. To find
 *  anagrams of a word, we find all the words from the dictionary which have the same occurrence list. We transform the sentence into its
 *  occurrence list, then try to extract any subset of characters from it to see if we can form any meaningful words. From the remaining
 *  characters we solve the problem recuresively and then combine all the meaningful words we have found with the recursive solution.
 *  
 *  Take the sentence 'You olive' as an example. Lets represent this sentence as an occurrence list of characters 'eiloouvy'. We start by
 *  suvtracting some subset of the characters, say 'i'. We are left with the characters 'eloouvy'
 *  
 *  Looking into the dictionary we see that 'i' correspons to word 'I' in the English language, so we found one menaingful word. We now
 *  solve te problem recursively for the rest of the characters 'eloouvy' and obtain a list of solutions 'List(List(love, you), List(you,
 *  love))'. We can combine 'I' with that list to obtain sentecnes 'I love you' and 'I you love', which are both valid anagrams. 
 * 
 */
object Anagrams {

  /** A word is simply a `String`.
   *  
   *  Words contain lowercase and uppercase chracters, and no whitespace, punctuation or other special characters.
   *   
   */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Characters that do not appear in the sentence do not appear in the occurrence list either.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = w.groupBy(char => char.toLower).map(pair => (pair._1, pair._2.length())).toList.sorted

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(word => wordOccurrences(word)).withDefaultValue(Nil)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {

    val charsCombinations = occurrences.flatMap(pair => (0 until pair._2 + 1) map (i => (pair._1, i)))

    (0 until (occurrences.length + 1)).flatMap(i => charsCombinations.combinations(i).map(seq => seq.sorted))
      .map(seq => seq.filter(elem => elem._2 != 0)).filterNot(seq => (seq.size != seq.map(elem => elem._1).toSet.size)).toSet.toList
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = x.toMap
    val yMap = y.toMap.withDefaultValue(0)
    xMap.map(elem => if (yMap.apply(elem._1) > 0) (elem._1, xMap(elem._1) - yMap(elem._1)) else elem).filter(elem => elem._2 != 0).toList
        .sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
        
    def findAnagrams(x: Occurrences): List[Sentence] = {
      if (x == Nil)
        List(Nil)
      else 
        for {
          
          y <- combinations(x)
          
          word <- dictionaryByOccurrences(y)
          
          z = subtract(x, y)  
          sentence <- findAnagrams(z)

        }
        yield word :: sentence
    }

    findAnagrams(sentenceOccurrences(sentence))  
  }
  /**
   * Instead of enlisting all the combinations, save the results obtained the first time when compute the anagrams for an occurrence list, 
   * and use the stored result if need the same result a second time.
   */
  def sentenceAnagramsMemo(sentence: Sentence): List[Sentence] = {
    val anagramsCache = collection.mutable.Map[Occurrences, List[Sentence]]().withDefaultValue(List(Nil))
    def findAnagrams(x: Occurrences): List[Sentence] = {
      for {
        y <- combinations(x)
        word <- dictionaryByOccurrences(y)
                  
                
        sentence <- cacheAnagrams(subtract(x, y))
      } 

      yield word :: sentence
    }
    def cacheAnagrams(x: Occurrences): List[Sentence] = {
      if (x == Nil) List(Nil)
      else if (anagramsCache(x) != List(Nil)) anagramsCache(x)
      
      else { 
        

        anagramsCache.update(x, findAnagrams(x))

        anagramsCache(x)
      }
    }
    
    cacheAnagrams(sentenceOccurrences(sentence)) 

  }
}
