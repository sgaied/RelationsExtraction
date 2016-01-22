

import java.util.Properties


import edu.stanford.nlp.ie.NERClassifierCombiner
import edu.stanford.nlp.io.EncodingPrintWriter.out
import edu.stanford.nlp.io.IOUtils
import edu.stanford.nlp.ling.CoreAnnotations.{NamedEntityTagAnnotation, TokensAnnotation, SentencesAnnotation}
import edu.stanford.nlp.ling.{CoreAnnotations, IndexedWord}
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation
import edu.stanford.nlp.trees.{GrammaticalRelation, UniversalEnglishGrammaticalRelations}

import scala.collection.JavaConversions._


/**
  * Created by yana on 07/01/2016.
  */
object testRelation {

  private var supplier = ""
  private var receiver = ""
  private var theme = ""
  private var root = ""

  private val set_modifiers = Set(UniversalEnglishGrammaticalRelations.ADJECTIVAL_MODIFIER,
    UniversalEnglishGrammaticalRelations.COMPOUND_MODIFIER,
    UniversalEnglishGrammaticalRelations.NUMERIC_MODIFIER,
    UniversalEnglishGrammaticalRelations.getNmod("of"))

  private val location_modifiers_set = Set(UniversalEnglishGrammaticalRelations.getNmod("in"),
    UniversalEnglishGrammaticalRelations.getNmod("at"), UniversalEnglishGrammaticalRelations.getNmod("from"))

  private val outgoingVerbsSet = Set("pipe", "supply", "export", "send", "provide", "render", "distribute", "sell", "ply",
    "deliver", "transport", "transfer", "transmit", "channel", "send")
  private val incomingVerbsSet = Set("receive", "get", "obtain", "incur ", "acquire", "buy", "purchase", "charter", "take",
    "bring", "source", "gather", "collect", "import", "extract", "derive", "procure")
  private val ambiguousVerbsSet = Set("ship")
  private val arrivalVerbsSet = Set("come", "arrive", "get")
  private val usageVerbsSet = Set("use", "consume", "enjoy", "benefit", "employ", "apply", "exploit", "tap", "utilize", "")

  /**
    * @param semGraph Represents a semantic graph of a sentence or document, with IndexedWord objects for nodes.
    * @return True if the semGraph is passive
    */
  def isActive(semGraph: SemanticGraph): Boolean = semGraph.findAllRelns(UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT).nonEmpty

  def setSupplier(supplier: String): Unit = {
    this.supplier = supplier
  }

  def setReceiver(receiver: String): Unit = {
    this.receiver = receiver
  }

  def setTheme(theme: String): Unit = {
    this.theme = theme
  }

  def getSupplier(): String = {
    this.supplier
  }

  def getReceiver(): String = {
    this.receiver
  }

  def getTheme(): String = {
    this.theme
  }


  /**
    * @param semGraph Represents a semantic graph of a sentence or document, with IndexedWord objects for nodes.
    * @return True if the semGraph is passive
    */
  def isPassive(semGraph: SemanticGraph): Boolean = {
    semGraph.findAllRelns(UniversalEnglishGrammaticalRelations.NOMINAL_PASSIVE_SUBJECT).nonEmpty
  }

  /**
    *
    * @param semanticGraph Represents a semantic graph of a sentence or document, with IndexedWord objects for nodes.
    * @param index         could be a supplier, a theme, a receiver
    * @return string in the SemGraph after index
    */
  def getStrFromIndex(semanticGraph: SemanticGraph, index: IndexedWord): String = {
    //TODO Should use option or Some
    if (index != null) {
      val indexModifiers = semanticGraph.getChildrenWithRelns(index, set_modifiers)
      val indexFull = indexModifiers + index
      val indexFullSorted = indexFull.toList.sortWith(_.index() < _.index())
      indexFullSorted.map(f => f.word()).mkString(" ")
    }
    else {
      "" // should disapear with the null
    }
  }

  def getSupplierNmod(semGraph: SemanticGraph, supplier: IndexedWord): String = {
    if (supplier != null) {
      val supplierModifiers = semGraph.getChildrenWithRelns(supplier, set_modifiers)

      val supplierNmodLocation = semGraph.getChildrenWithRelns(supplier, location_modifiers_set)
      val supplierNmodLocationModifiers = supplierNmodLocation.flatMap(f => semGraph.getChildrenWithRelns(f, set_modifiers + UniversalEnglishGrammaticalRelations.CASE_MARKER))

      val supplierFull = supplierModifiers + supplier ++ supplierNmodLocation ++ supplierNmodLocationModifiers
      val supplierFullSorted = supplierFull.toList.sortWith(_.index() < _.index())
      supplierFullSorted.map(f => f.word()).mkString(" ")
    }
    else {
      ""
    }
  }

  def getTupleRelation(root: IndexedWord, supplier: String, receiver: String, theme: String): (String, String, String, String) = {
    (root.lemma(), supplier, receiver, theme)
  }

  /**
    * Determine the voice of the sentence :
    * If the sentence is in active voice, a 'nsubj' dependency should exist.
    * If the sentence is in passive voice a 'nsubjpass' dependency should exist
    *
    * @param semGraph Represents a semantic graph of a sentence or document, with IndexedWord objects for nodes.
    * @return
    */
  def relationParsing(semGraph: SemanticGraph): (String, String, String, String) = {


    val active = isActive(semGraph)
    val passive = isPassive(semGraph)

    var res: (String, String, String, String) = ("", "", "", "")
    val root = semGraph.getFirstRoot

    //var supplierFullString: String = ""
    //var receiverFullString: String = ""
    //var themeFullString: String = ""

    setSupplier("")
    setReceiver("")
    setTheme("")


    //The following code assigns supplier, receiver and theme via sentence tree parsing
    // and as a function of the root verb type
    //TODO Replace all the null testing
    // IF the verb is coupula (eg., "is available from"), the theme head word will be located in the nominal subject
    // of the root verb and the supplier will be connected to root via a modifier : "nmod:from".
    // Function getStrFromIndex enriches the theme and supplier head words by the modifiers from the set set_modifiers
    // Function getTupleRelation creates a list from the root, supplierFullString, receiverFullString,themeFullString
    if (getCopula(semGraph, root) != null) {
      setTheme(getStrFromIndex(semGraph, semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT)))
      setSupplier(getStrFromIndex(semGraph, semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))))
    }

    // Active Voice
    else if (active) {

      if (outgoingVerbsSet.contains(root.lemma())) {
        // IF voice of the sentence is active and the root verb belongs to the outgoingVerbsSet,
        // the Sentence may look either like "Supplier supplies receiver WITH theme"
        // or "Supplier supplies theme TO receiver"
        // In both cases, the supplier head word will the root's nominal subject .
        // In the first case, receiver's head word will be root's direct object, while theme will be receiver's Nmod:with
        // In the second case, theme's head word will be root's direct object, while the receiver head word will
        // be connected to theme via Nmod:to or Nmod:for.
        // Which case we are in depends on whether the direct object of root has an Nmod:with dependency
        // Function getStrFromIndex enriches the theme and supplier head words by the modifiers from the set
        // set_modifiers thus transforming the respective head words into the full entities.
        // Function getTupleRelation creates a list from the
        // root, supplierFullString, receiverFullString,themeFullString

        val obj = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.DIRECT_OBJECT)

        if (obj != null) {
          val objNmod = semGraph.getChildWithReln(obj, UniversalEnglishGrammaticalRelations.getNmod("with"))

          if (objNmod == null) {
            //theme = obj
            //themeFullString = getStrFromIndex(semGraph, theme)
            val receiverModifiersSet = Set(UniversalEnglishGrammaticalRelations.getNmod("to"), UniversalEnglishGrammaticalRelations.getNmod("for"))
            val themeReceivers = semGraph.getChildrenWithRelns(obj, receiverModifiersSet)
            if (themeReceivers.size() > 0) {
              val themeReceiversModifiers = themeReceivers.flatMap(f => semGraph.getChildrenWithRelns(f, set_modifiers))
              val receiversFull = themeReceivers ++ themeReceiversModifiers
              val receiversFullSorted = receiversFull.toList.sortWith(_.index() < _.index())
              setReceiver(receiversFullSorted.map(f => f.word()).mkString(" "))
              setTheme(getStrFromIndex(semGraph, obj))
            }
          }
          else {
            setTheme(getStrFromIndex(semGraph, objNmod))
            setReceiver(getStrFromIndex(semGraph, obj))
          }
        }
        setSupplier(getStrFromIndex(semGraph, semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT)))


      }

      // should it be else if ? otherwise res might be rewritten
      if (incomingVerbsSet.contains(root.lemma()) | usageVerbsSet.contains(root.lemma())) {
        // IF voice of the sentence is active and the root verb belongs to the incomingVerbsSet or usageVerbsSet,
        // the Sentence may look either like "Receiver obtains theme from Supplier"
        // or "Receiver uses theme from Supplier"
        // In both cases, the receiver head word will the root's nominal subject .
        // The theme head word will be root's direct object, while supplier's head word will be root's Nmod:from
        // Function getStrFromIndex enriches the theme and supplier head words by the modifiers from the set
        // set_modifiers thus transforming the respective head words into the full entities.
        // Function getTupleRelation creates a list from the
        // root, supplierFullString, receiverFullString,themeFullString


        val receiver = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT)
        val theme = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.DIRECT_OBJECT)
        val supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))

        setReceiver(getStrFromIndex(semGraph, receiver))
        setTheme(getStrFromIndex(semGraph, theme))
        setSupplier(getSupplierNmod(semGraph, supplier))

      }

      // Could this be a test with copula ? - NON
      if (arrivalVerbsSet.contains(root.lemma())) {
        val theme = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT)
        val supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))

        setTheme(getStrFromIndex(semGraph, theme))
        setSupplier(getSupplierNmod(semGraph, supplier))
        setReceiver("")
      }
    }

    // Passive voice
    else if (passive) {

      setTheme(getStrFromIndex(semGraph, semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_PASSIVE_SUBJECT)))

      if (outgoingVerbsSet.contains(root.lemma())) {
        // IF voice of the sentence is passive and the root verb belongs to the outgoingVerbsSet,
        // the Sentence looks like "Theme is supplied to receiver by/from supplier"
        // The theme's head word will the root's nominal passive subject .
        // Receiver's head word will be connected to root as Nmod:to
        // Supplier will be connected to root as an agent (Nmod:by)root's direct object, while the receiver head word will
        // be connected to theme via Nmod:to or Nmod:for.
        // Function getStrFromIndex enriches the theme and supplier head words by the modifiers from the set
        // set_modifiers thus transforming the respective head words into the full entities.
        // Function getTupleRelation creates a list from the
        // root, supplierFullString, receiverFullString,themeFullString

        var supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("agent"))
        val receiver = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("to"))
        if (supplier == null) {
          supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))
        }

        setSupplier(getStrFromIndex(semGraph, supplier))
        setReceiver(getStrFromIndex(semGraph, receiver))
      }
      // should it be else if ? otherwise res might be rewritten
      else if (incomingVerbsSet.contains(root.lemma())) {
        // IF voice of the sentence is passive and the root verb belongs to the incomingVerbsSet,
        // the Sentence looks like "Theme is obtained by receiver from supplier"
        // The theme's head word will the root's nominal passive subject .
        // Receiver's head word will be connected to root as an agent (Nmod:by)
        // Supplier will be connected to root as an agent (Nmod:from)
        // Function getStrFromIndex enriches the theme and supplier head words by the modifiers from the set
        // set_modifiers thus transforming the respective head words into the full entities.
        // Function getTupleRelation creates a list from the
        // root, supplierFullString, receiverFullString,themeFullString

        setSupplier(getStrFromIndex(semGraph, semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))))
        setReceiver(getStrFromIndex(semGraph, semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("agent"))))

      }
    }
    // if root verb lemme belongs to the list of undetermined verbs (neither incoming nor outgoing)
    //
    else if (ambiguousVerbsSet.contains(root.lemma())) {
      // if root verb lemme belongs to the list of undetermined verbs (neither incoming nor outgoing)
      // ex., "ship" that can be both incoming ("Receiver has shipped the theme from Supplier")
      // or outgoing ("Supplier shipped the theme to the receiver)
      // Which case takes place is determined by theme head word modifier ("from" or "to")
      // In both cases, theme head word is a dependent of the root
      // In the first case, Supplier head word is linked to the theme by Nmod:from, while receiver is
      // the root's compound modifier.
      // In the second case, supplier's head word is the root's compound modifier, while
      // receiver is linked to theme via modifier Nmod:to
      val nom_subj = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.COMPOUND_MODIFIER)
      val theme = semGraph.getChildWithReln(root, GrammaticalRelation.DEPENDENT)
      val theme_from = semGraph.getChildWithReln(theme, UniversalEnglishGrammaticalRelations.getNmod("from"))
      val theme_to = semGraph.getChildWithReln(theme, UniversalEnglishGrammaticalRelations.getNmod("to"))

      if (theme != null) {

        setTheme(getStrFromIndex(semGraph, theme))
        if (theme_from != null) {
          setSupplier(getSupplierNmod(semGraph, theme_from))
          setReceiver(getStrFromIndex(semGraph, nom_subj))
        }
        else if (theme_to != null) {
          setSupplier(getSupplierNmod(semGraph, nom_subj))
          setReceiver(getStrFromIndex(semGraph, theme_to))

        }
      }

    }

    res = getTupleRelation(root, getSupplier(), getReceiver(), getTheme())

    res
  }

  def getRelationships(s: String, pipeline: StanfordCoreNLP): String = {

    val document = new Annotation(s)
    pipeline.annotate(document)
    // Parsing document into sentences
    val sentences = document.get(classOf[SentencesAnnotation])
    //creating a list of Semantic Graphs :  to each sentence there is a semantic graph associated
    var CollapsedSentenceDep = List[SemanticGraph]()
    asScalaBuffer(sentences.reverse).foreach(CollapsedSentenceDep ::= _.get(classOf[CollapsedCCProcessedDependenciesAnnotation]))
    var Relations = List[(String, String, String, String)]()
    Relations = CollapsedSentenceDep.map(sg => relationParsing(sg))
    //val CollapsedSentenceDepString = CollapsedSentenceDep.map(sg => sg.toString)
    //val pair = sentences zip CollapsedSentenceDepString zip Relations
    val pair = sentences zip Relations
    //Relations.mkString("=>")
    pair.mkString("=>")

  }

  def getCopula(semGraph: SemanticGraph, root: IndexedWord): IndexedWord = {


    val copula = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.COPULA)

    copula

  }

  def main(args: Array[String]) {

    val properties = new Properties()

    val serializedClassifier1 = "classifiers/english.all.3class.distsim.crf.ser.gz"
    val serializedClassifier = "classifiers/CrudeSupply2.classifier.ser.gz"
    val classifier = new NERClassifierCombiner(false, false, serializedClassifier, serializedClassifier1)

    properties.put("ner.model", classifier)
    properties.put("regexner.mapping.header", "pattern, ner")
    properties.put("regexner.backgroundSymbol","O, LOCATION")
    properties.put("regexner.mapping", "classifiers/classeur1.txt")

    properties.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner")
    val corenlp = new StanfordCoreNLP(properties)
    val percent = "Gaz de France provides around 30-40% of the fuel gas requirements for the site. Crude Oil is available from the Kumkol Oil Field. The refinery processes local Hassi Messaoud Crude Oil, which is supplied by pipeline."
    //print(getRelationships(percent, corenlp))
    val fileContents = IOUtils.slurpFile("src/main/resources/SupplyExample")
    var document = new Annotation(fileContents)
    corenlp.annotate(document)



    val sentences = document.get(classOf[SentencesAnnotation])
    for (sentence <- sentences) {
      for (token <- sentence.get(classOf[TokensAnnotation])) {
        System.out.print(token.word() + "/" + token.get(classOf[NamedEntityTagAnnotation]) + ' ')
      }
      println()
    }
  }
}