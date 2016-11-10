package adt

import argonaut.Argonaut._
import argonaut._

case class Blob(name: String, inner: InnerBlobType)
case class Element(id: String)

sealed trait InnerBlobType
case class InnerBlob(name: String) extends InnerBlobType
case class InnerBlobWithElements(name: String, elements: List[Element]) extends InnerBlobType

object Blob {
  def innerBlob(name: String): InnerBlobType = InnerBlob(name)
  def innerBlobWithElements (name: String, elements: List[Element]): InnerBlobType =
    InnerBlobWithElements(name, elements)

  implicit val decodeInner: DecodeJson[InnerBlobType] =
    DecodeJson(c =>
      c.as(jdecode2L(innerBlobWithElements)("name", "elements")) |||
      c.as(jdecode1L(innerBlob)("name"))
    )

  implicit val decodeElement = jdecode1L(Element)("id")
  implicit val decodeBlob = jdecode2L(Blob.apply)("name", "inner")
}
