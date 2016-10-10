/*
    This file is part of "argonaut-tagged-union"
    Copyright (C) 2016 Toby Thain, toby@telegraphics.com.au
    ...with help from dibblego and aarvar in #scalaz

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

package adt

import argonaut._, Argonaut._

sealed trait Message

final case class Error(description: String) extends Message

final case class Item(id: Int, contents: String) extends Message

object Message {
  def error(description: String): Message      = Error(description)
  def item(id: Int, contents: String): Message = Item(id, contents)

  implicit def encode = EncodeJson[Message]{
    case Error(d)    => Json("error" -> Json("description" := d))
    case Item(id, c) => Json("item"  -> Json("id"       := id,
                                             "contents" := c))
  }

  implicit def decode: DecodeJson[Message] =
    DecodeJson(c =>
      (c --\ "error").as(jdecode1L(error)("description")) |||
      (c --\ "item").as(jdecode2L(item)("id", "contents"))
    )
}
