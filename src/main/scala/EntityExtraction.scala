import java.util.Properties

import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import org.apache.spark.{SparkContext, SparkConf}

/**
  * Created by nico on 28/12/2015.
  */
object EntityExtraction {

  // Proably need to change  the iterator[(String, String)] to match the output results of each rdd map ?
  // In this example returning for each map a tuple of 2 strings
  def extractNER(p : Iterator[String]): Iterator[(String, String)] = {

    // setup per partition
    // creates a StanfordCoreNLP object, with POS tagging, lemmatization, NER, parsing, and coreference resolution
    val props = new Properties()
    props.setProperty("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
    val pipeline = new StanfordCoreNLP(props)


    // run all Annotators on this text

    p.map(line => {
      val document = new Annotation(line)
      pipeline.annotate(document)
      // do something else with the document returning always ("toto", "tata")
      ("toto", "tata")

    })
  }

  def main (args: Array[String]){

    val logFile = "src/main/resources/sentences.txt" // Should be some file on your system
    val conf = new SparkConf().setAppName("Entity Extraction").setMaster("local")
    val sc = new SparkContext(conf)
    val input = sc.textFile(logFile, 2).cache()
    // change output directory
    input.mapPartitions(p => extractNER(p)).map(x => x._2).saveAsTextFile("/Users/nico/Desktop/test")
  }

}
