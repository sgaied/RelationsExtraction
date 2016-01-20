import java.util.Properties

import edu.stanford.nlp.pipeline.StanfordCoreNLP
import org.scalatest.FlatSpec

/**
  * Created by nico on 17/01/2016.
  */

class RelationSpec extends FlatSpec {

  val relation = new Relation()
  val properties = new Properties()
  properties.setProperty("annotators", "tokenize, ssplit, parse, lemma")
  val corenlp = new StanfordCoreNLP(properties)

  //println(relation.getRelationships(refinery, corenlp))

  //  "A passive graph" should "be true with isPassive " {
  //    assert(true == relation.isPassive(new SemanticGraph()))
  //  }

  /*
    "Yana should explain getRelationships to nico" should "be the following sentence" in {
      val gdf = "Gaz de France provides the site with its fuel gas requirements."
      val gdfRln = "(Gaz de France provides around 30-40% of the fuel gas requirements for the site.,(provide,Gaz de France,,))"

      assert(gdfRln == relation.getRelationships(gdf, corenlp))
    }
  */

  "Yana should explain getRelationships to nico pourcent" should "be the following sentence" in {
    val pourcent = "Gaz de France provides around 30-40% of the fuel gas requirements for the site."
    val pourcentRln = "(Gaz de France provides around 30-40% of the fuel gas requirements for the site.,(provide,Gaz de France,,))"

    assert(pourcentRln == relation.getRelationships(pourcent, corenlp))

  }

  "Yana should explain getRelationships to nico crude" should "be the following sentence" in {
    val crude = "Crude Oil is available from the Kumkol Oil Field."
    val crudeRln = "(Crude Oil is available from the Kumkol Oil Field.,(available,Kumkol Oil Field,,Crude Oil))"

    assert(crudeRln == relation.getRelationships(crude, corenlp))
  }

  "Yana should explain getRelationships to nico refinery" should "be the following sentence" in {
    val refinery = "The refinery processes local Hassi Messaoud Crude Oil, which is supplied by pipeline."
    val refineryRln = "(The refinery processes local Hassi Messaoud Crude Oil, which is supplied by pipeline.,(process,,,))"
    assert(refineryRln == relation.getRelationships(refinery, corenlp))
  }

}
