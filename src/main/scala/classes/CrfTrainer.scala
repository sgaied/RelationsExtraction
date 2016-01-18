package main.scala.classes

import java.util
import java.util.StringTokenizer

import edu.stanford.nlp.ie.crf.CRFClassifier
import edu.stanford.nlp.sequences.SeqClassifierFlags


object FeatureConfig extends Enumeration {

  val DEFAULT: IndexedSeq[(String, Either[Int, Boolean])] = IndexedSeq(
    ("maxLeft", Left(1)),
    ("maxRight", Left(6)),
    ("useClassFeature", Right(true)),
    ("useWord", Right(true)),
    ("useWordTag", Right(true)),
    ("useNGrams", Right(false)),
    //("noMidNGrams", Right(true)),
    //("maxNGramLeng", Left(6)),
    ("usePrev", Right(true)),
    ("useNext", Right(true)),
    ("useDisjunctive", Right(true)),
    ("useSequences", Right(true)),
    ("usePrevSequences", Right(true)),
    ("useLongSequences", Right(true)),
    ("useNextSequences", Right(true)),
    ("useTypeSeqs", Right(true)),
    ("useTypeSeqs2", Right(true)),
    ("useTypeySequences", Right(true)),
    ("useChunks", Right(true)),
    ("useNPGovernor", Right(true)),
    ("useHeadGov", Right(true)),
    ("useLemmas", Right(true)),
    ("usePrevNextLemmas", Right(true))

  )

}


/*
  Train a supervised NER model, given an annotated training set.
 */
object CrfTrainer {

  private[this] def setFlags(trainingDataLocation: String,
                             modelSaveLocation: String,
                             features: IndexedSeq[(String, Either[Int, Boolean])],
                             gazLocation: String,
                             auxClissifierLocation: String): SeqClassifierFlags = {

    val flags = new SeqClassifierFlags()

    flags.trainFile = trainingDataLocation
    flags.serializeTo = modelSaveLocation
    flags.map = "word=0,answer=1"
    flags.loadAuxClassifier =auxClissifierLocation

    val gazLoc = new util.ArrayList[String]()

    if (gazLocation != "") {
      val st = new StringTokenizer(gazLocation, " ,;\t")

      while (st.hasMoreTokens) {
        gazLoc.add(st.nextToken())
      }

      flags.gazettes = gazLoc
      flags.useGazettes = true
    }

    //Set all features values
    for (feature <- features) {

      //Non-exhaustive matching because we know what type they are, so ignore warnings
      feature match {
        case ("maxLeft", x) => flags.maxLeft = x match {
          case Left(ml) => ml
        }
        case ("useLemmas", x) => flags.useLemmas = x match {
          case Right(ml) => ml
        }
        case ("usePrevNextLemmas", x) => flags.usePrevNextLemmas = x match {
          case Right(ml) => ml
        }



        case ("maxRight", x) => flags.maxRight = x match {
              case Left(ml) => ml
        }
        case ("useClassFeature", x) => flags.useClassFeature = x match {
          case Right(ucf) => ucf
        }
        case ("useWord", x) => flags.useWord = x match {
          case Right(uw) => uw
        }

        case ("useWordTag", x) => flags.useWordTag = x match {
          case Right(uwt) => uwt
        }
        case ("useChunks", x) => flags.useChunks = x match {
          case Right(uc) => uc
        }

        case ("useNGrams", x) => flags.useWord = x match {
          case Right(ung) => ung
        }
        case ("noMidNGrams", x) => flags.noMidNGrams = x match {
          case Right(nmng) => nmng
        }
        case ("useNPGovernor", x) => flags.useNPGovernor = x match {
          case Right(nmng) => nmng
        }

        case ("useHeadGov", x) => flags.useHeadGov = x match {
          case Right(nmng) => nmng
        }
        case ("maxNGramLeng", x) => flags.maxNGramLeng = x match {
          case Left(mngl) => mngl
        }
        case ("usePrev", x) => flags.usePrev = x match {
          case Right(up) => up
        }
        case ("useNext", x) => flags.useNext = x match {
          case Right(un) => un
        }
        case ("useDisjunctive", x) => flags.useDisjunctive = x match {
          case Right(un) => un
        }
        case ("useSequences", x) => flags.useSequences = x match {
          case Right(us) => us
        }
        case ("usePrevSequences", x) => flags.usePrevSequences = x match {
          case Right(ups) => ups
        }
        case ("useLongSequences", x) => flags.useLongSequences = x match {
          case Right(uls) => uls
        }

        case ("useNextSequences", x) => flags.useNextSequences = x match {
          case Right(uns) => uns
        }


        case ("useTypeSeqs", x) => flags.useTypeSeqs = x match {
          case Right(uts) => uts
        }
        case ("useTypeSeqs2", x) => flags.useTypeSeqs2 = x match {
          case Right(uts2) => uts2
        }
        case ("useTypeySequences", x) => flags.useTypeySequences = x match {
          case Right(utys) => utys
        }
      }
    }

    flags
  }

  def trainClassifier(trainingDataLocation: String,
                      modelSaveLocation: String,
                      features: IndexedSeq[(String, Either[Int, Boolean])] = FeatureConfig.DEFAULT,
                      gazLocation: String = "",
                      auxClissifierLocation: String = "") = {

    val nerClassifier = new CRFClassifier(
      setFlags(trainingDataLocation,
        modelSaveLocation,
        features,
        gazLocation,
        auxClissifierLocation)
    )

    nerClassifier.train()
    nerClassifier.serializeClassifier(modelSaveLocation)
  }


}
