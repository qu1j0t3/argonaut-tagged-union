/*
    This file is part of "argonaut-tagged-union"
    Copyright (C) 2016 Toby Thain, toby@telegraphics.com.au
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

scalaVersion := "2.11.8"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.1"

// This version is chosen to be compatible with scalaz 7.1.x
// to avoid a java.lang.IncompatibleClassChangeError
libraryDependencies += "org.specs2" % "specs2-core_2.11" % "3.8.5-scalaz-7.1.10" % Test

scalacOptions in Test ++= Seq("-Yrangepos")
