Project: TestNER
Author:Selsabil Gaied
Creation Date: 12/01/2016
Last updated: 15/01/2016 by Selsabil Gaied

Objective:Train and use a supervised NER model, given an annotated training set.


Files:

1- main.ner.Train.scala: //CrfTrainer.trainClassifier("FileName","SerializeTo")

2- main.ner.interactiveTraining.scala:
  An interactive app that presents the user with a document and allows them to annotate each token as a named entity
  type.  By convention, an annotation of "O" means the token is not a named entity.

  Documents are presented to the user line-by-line.  For convenience, hitting "ENTER" indicates the current token is
  not a named entity and is automatically annotated with a "O".  Also for convenience, if the user sees there are no
  named entities on the current line, they may type "next", in which case all remaining tokens on the current line are
  automatically annotated with "O" and the next line is presented.

  All annotations are written to file in a format that can be read by the Stanford NER tool for training.

  Input: (Required)

    args(0): File path+name to write annotations
    val annotationsWriteFile = args(0)
    args(1):Location of directory containing documents on which to perform annotations.
    val documentDirectory = args(1)

3- main.ner.NERDemo.scala: Stanford NER Demo file rewritten in Scala

    Use:
    - If arguments aren't specified, they default to classifiers/english.muc.7class.distsim.crf.ser.gz
    and some hardcoded sample text.
    - Or: args(0): classifier
    - Else, input required: args(0): classifier
                            args(1): textFile
        Ex., classifiers/CrudeSupply2.classifier.ser.gz src/main/ressources/SupplyExample.txt
4- run.sh (to be completed)
