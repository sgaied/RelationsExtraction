import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.semgraph.SemanticGraphCoreAnnotations.CollapsedCCProcessedDependenciesAnnotation
import edu.stanford.nlp.trees.UniversalEnglishGrammaticalRelations

import scala.collection.JavaConversions._


/**
  * Created by yana on 07/01/2016.
  */
class Relation {

  private val set_modifiers = Set(UniversalEnglishGrammaticalRelations.ADJECTIVAL_MODIFIER,
    UniversalEnglishGrammaticalRelations.COMPOUND_MODIFIER,
    UniversalEnglishGrammaticalRelations.NUMERIC_MODIFIER,
    UniversalEnglishGrammaticalRelations.getNmod("of"))

  private val location_modifiers_set = Set(UniversalEnglishGrammaticalRelations.getNmod("in"),
    UniversalEnglishGrammaticalRelations.getNmod("at"), UniversalEnglishGrammaticalRelations.getNmod("from"))
  /**
    * @param semGraph
    * @return True if the semGraph is passive
    */
  def isActive(semGraph: SemanticGraph) : Boolean = {
    semGraph.findAllRelns(UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT).length > 0
  }

  /**
    * @param semGraph
    * @return True if the semGraph is passive
    */
  def isPassive(semGraph: SemanticGraph) : Boolean = {
    semGraph.findAllRelns(UniversalEnglishGrammaticalRelations.NOMINAL_PASSIVE_SUBJECT).length > 0
  }

  /**
    *
    * @param semanticGraph
    * @param index could be a supplier, a theme, a receiver
    * @return
    */
  def getStrFromIndex(semanticGraph: SemanticGraph, index: IndexedWord) : String = {
    //TODO Should use option or Some
    if (index != null) {
      val index_modifiers = semanticGraph.getChildrenWithRelns(index, set_modifiers)
      val index_full = index_modifiers + index
      val index_full_sorted = index_full.toList.sortWith(_.index() < _.index())
      index_full_sorted.map(f => f.word()).mkString(" ")
    }
    else {
      "" // should disapear with the null
    }
  }

  def  getSupplierNmod(semGraph: SemanticGraph, supplier: IndexedWord) : String = {
    if (supplier != null) {
      val supplier_modifiers = semGraph.getChildrenWithRelns(supplier, set_modifiers)

      val supplier_nmod_location = semGraph.getChildrenWithRelns(supplier, location_modifiers_set)
      val supplier_nmod_location_modifiers = supplier_nmod_location.flatMap(f => semGraph.getChildrenWithRelns(f, set_modifiers + UniversalEnglishGrammaticalRelations.CASE_MARKER))

      val supplier_full = supplier_modifiers + supplier ++ supplier_nmod_location ++ supplier_nmod_location_modifiers
      val supplier_full_sorted = supplier_full.toList.sortWith(_.index() < _.index())
      supplier_full_sorted.map(f => f.word()).mkString(" ")
    }
    else {
      ""
    }
  }

  /**
    * Determine the voice of the sentence :
    * If the sentence is in active voice, a 'nsubj' dependency should exist.
    * If the sentence is in passive voice a 'nsubjpass' dependency should exist
    *
    * @param semGraph
    * @return
    */
  def relationParsing(semGraph: SemanticGraph): (String, String, String, String) = {

    val active = isActive(semGraph)
    val passive = isPassive(semGraph)

    var res: (String, String, String, String) = ("", "", "", "")
    val root = semGraph.getFirstRoot()

    val set_outgoing_verbs = Set("pipe", "supply", "export", "send", "provide", "render", "distribute", "sell", "ply",
      "deliver", "transport", "transfer", "transmit", "channel", "send")
    val incoming_verbs_set = Set("receive", "get", "obtain", "incur ", "acquire", "buy", "purchase", "charter", "take",
      "bring", "source", "gather", "collect", "import", "extract", "derive", "procure")
    val ambiguous_verbs_set = Set("ship")
    val arrival_verbs_set = Set("come", "arrive", "get")
    val usage_verbs_set = Set("use", "consume", "enjoy", "benefit", "employ", "apply", "exploit", "tap", "utilize", "")

    var supplier_full_string: String = ""
    var receiver_full_string: String = ""
    var theme_full_string: String = ""

    val copula = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.COPULA)
    if (copula != null) {
      val children = semGraph.childRelns(root)
      val theme = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT)
      val supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))


      theme_full_string = getStrFromIndex(semGraph, theme)
      supplier_full_string = getStrFromIndex(semGraph, supplier)
      res = (root.lemma(), supplier_full_string, receiver_full_string, theme_full_string)
    }

    // Active Voice
    if (active) {
      if (set_outgoing_verbs.contains(root.lemma())) {
        val children = semGraph.childRelns(root)
        val supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT)
        val obj = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.DIRECT_OBJECT)
        if (obj != null) {
          val obj_nmod = semGraph.getChildWithReln(obj, UniversalEnglishGrammaticalRelations.getNmod("with"))

          if (obj_nmod == null) {
            val theme = obj
            theme_full_string = getStrFromIndex(semGraph, theme)

            val receiver_modifiers_set = Set(UniversalEnglishGrammaticalRelations.getNmod("to"), UniversalEnglishGrammaticalRelations.getNmod("for"))
            val theme_receivers = semGraph.getChildrenWithRelns(theme, receiver_modifiers_set)
            if (theme_receivers.size() > 0) {
              val theme_receivers_modifiers = theme_receivers.flatMap(f => semGraph.getChildrenWithRelns(f, set_modifiers))
              val receivers_full = theme_receivers ++ theme_receivers_modifiers
              val receivers_full_sorted = receivers_full.toList.sortWith(_.index() < _.index())
              receiver_full_string = receivers_full_sorted.map(f => f.word()).mkString(" ")
            }
          }

          if (obj_nmod != null) {
            val theme_receiver = obj
            receiver_full_string = getStrFromIndex(semGraph, theme_receiver)

            val theme = obj_nmod
            theme_full_string = getStrFromIndex(semGraph, theme)
          }

        }

        supplier_full_string = getStrFromIndex(semGraph, supplier)
        res = (root.lemma(), supplier_full_string, receiver_full_string, theme_full_string)
      }

      // should it be else if ? otherwise res might be rewritten
      if ((incoming_verbs_set.contains(root.lemma())) | usage_verbs_set.contains(root.lemma())) {

        val children = semGraph.childRelns(root)
        val receiver = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT)
        val theme = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.DIRECT_OBJECT)
        val supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))

        receiver_full_string = getStrFromIndex(semGraph, receiver)
        theme_full_string = getStrFromIndex(semGraph, theme)
        supplier_full_string = getSupplierNmod(semGraph, supplier)

        res = (root.lemma(), supplier_full_string, receiver_full_string, theme_full_string)
      }

      if (arrival_verbs_set.contains(root.lemma())) {
        val children = semGraph.childRelns(root)
        val theme = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT)
        val supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))

        theme_full_string = getStrFromIndex(semGraph, theme)
        supplier_full_string = getSupplierNmod(semGraph, supplier)
        res = (root.lemma(), supplier_full_string, "", theme_full_string)
      }
    }

    // Passive voice
    if (passive) {
      val root = semGraph.getFirstRoot()
      val theme = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.NOMINAL_PASSIVE_SUBJECT)

      if (set_outgoing_verbs.contains(root.lemma())) {

        val children = semGraph.childRelns(root)

        val supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("agent"))
        val receiver = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("to"))

        theme_full_string = getStrFromIndex(semGraph, theme)
        supplier_full_string = getStrFromIndex(semGraph, supplier)
        receiver_full_string = getStrFromIndex(semGraph, receiver)
        res = (root.lemma(), supplier_full_string, receiver_full_string, theme_full_string)
      }

      if (incoming_verbs_set.contains(root.lemma())) {
        val root = semGraph.getFirstRoot()
        val children = semGraph.childRelns(root)
        val supplier = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("from"))
        val receiver = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.getNmod("agent"))

        theme_full_string = getStrFromIndex(semGraph, theme)
        supplier_full_string = getStrFromIndex(semGraph, supplier)
        receiver_full_string = getStrFromIndex(semGraph, receiver)
        res = (root.lemma(), supplier_full_string, receiver_full_string, theme_full_string)
      }
    }

    // Undetermined
    if ((!active).&&(!passive)) {

      if (ambiguous_verbs_set.contains(root.lemma())) {
        import edu.stanford.nlp.trees.GrammaticalRelation
        val children = semGraph.childRelns(root)
        val nom_subj = semGraph.getChildWithReln(root, UniversalEnglishGrammaticalRelations.COMPOUND_MODIFIER)
        val theme = semGraph.getChildWithReln(root, GrammaticalRelation.DEPENDENT)
        val theme_from = semGraph.getChildWithReln(theme, UniversalEnglishGrammaticalRelations.getNmod("from"))
        val theme_to = semGraph.getChildWithReln(theme, UniversalEnglishGrammaticalRelations.getNmod("to"))

        if (theme != null) {
          if (theme_from != null) {
            val supplier = theme_from
            val receiver = nom_subj

            supplier_full_string = getSupplierNmod(semGraph, supplier)
            receiver_full_string = getStrFromIndex(semGraph, receiver)
            theme_full_string = getStrFromIndex(semGraph, theme)


          }
          else if (theme_from != null) {
            val supplier = nom_subj
            val receiver = theme_to

            supplier_full_string = getSupplierNmod(semGraph, supplier)
            receiver_full_string = getStrFromIndex(semGraph, receiver)
            theme_full_string = getStrFromIndex(semGraph, theme)
          }
        }

        res = (root.lemma(), supplier_full_string, receiver_full_string, theme_full_string)
      }
    }
    res
  }

  def getRelationships(s: String, pipeline: StanfordCoreNLP) : String = {

    val document = new Annotation(s)
    pipeline.annotate(document)
    val sentences = document.get(classOf[SentencesAnnotation])
    var CollapsedSentenceDep = List[SemanticGraph]()
    sentences.foreach(CollapsedSentenceDep ::= _.get(classOf[CollapsedCCProcessedDependenciesAnnotation]))
    var Relations = List[(String, String, String, String)]()
    Relations = CollapsedSentenceDep.map(sg => relationParsing(sg))
    val CollapsedSentenceDepString = CollapsedSentenceDep.toList.map(sg => sg.toString)
    val pair = sentences zip CollapsedSentenceDepString zip Relations
    pair.mkString("=>")

    //var SentenceTriples = List[Array[RelationTriple]]()
    //sentences.foreach(SentenceTriples ::= _.get(classOf[RelationTriplesAnnotation]))
    //val SentenceTriples = sentences.get(0).get(classOf[RelationTriplesAnnotation])

    /*
  Collection<RelationTriple> triples = sentence.get(NaturalLogicAnnotations.RelationTriplesAnnotation.class);
  // Print the triples
  for (RelationTriple triple : triples) {
    System.out.println(triple.confidence + "\t" +
      triple.subjectLemmaGloss() + "\t" +
      triple.relationLemmaGloss() + "\t" +
      triple.objectLemmaGloss());
  }
  */
  }
}
