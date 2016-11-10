package adt

import org.specs2.mutable.Specification

import argonaut._

class BlobSpec extends Specification {
  import Blob.decodeBlob

  val blobJson =
    """
      |{
      |  "name": "outer blob",
      |  "inner": {
      |    "name": "inner blob"
      |  }
      |}""".stripMargin
  val blobWithElements =
    """
      |{
      |  "name": "outer blob",
      |  "inner": {
      |    "name": "inner blob with elements",
      |    "elements": [ { "id": "lovely-element" },
      |                  { "id": "excellent-element" } ]
      |  }
      |}""".stripMargin

  "Argonaut" >> {
    "can decode Blob" in {
      Parse.decodeOption[Blob](blobJson).get.must_==(Blob("outer blob", InnerBlob("inner blob")))
    }
    "can decode Blob with Elements" in {
      Parse.decodeOption[Blob](blobWithElements).get.must_==(
        Blob("outer blob", InnerBlobWithElements("inner blob with elements",
          List(Element("lovely-element"), Element("excellent-element"))))
      )
    }
  }
}
