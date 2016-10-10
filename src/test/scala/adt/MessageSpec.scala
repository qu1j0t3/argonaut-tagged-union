package adt

import org.specs2.mutable.Specification
import org.specs2.matcher.DisjunctionMatchers

import argonaut._

class MessageSpec extends Specification with DisjunctionMatchers {

  val e = implicitly[EncodeJson[Message]]

  "Message JSON encoder should" >> {
    "encode Error correctly" in {
      e.encode(Error("foo")).nospaces must_== "{\"error\":{\"description\":\"foo\"}}"
    }

    "encode Item correctly" in {
      e.encode(Item(123, "foo")).nospaces must_== "{\"item\":{\"id\":123,\"contents\":\"foo\"}}"
    }

    "decode Error correctly" in {
      Parse.decode[Message]("{\"error\":{\"description\":\"foo\"}}") must be_\/-(Error("foo"))
    }

    "decode Item correctly" in {
      Parse.decode[Message]("{\"item\":{\"id\":123,\"contents\":\"foo\"}}") must be_\/-(Item(123, "foo"))
    }

    "not decode unknown tag" in {
      Parse.decode[Message]("{\"oops\":{\"description\":\"foo\"}}") must be_-\/
    }
  }

}
