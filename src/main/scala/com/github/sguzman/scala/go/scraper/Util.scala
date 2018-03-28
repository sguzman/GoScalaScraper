package com.github.sguzman.scala.go.scraper

import java.net.SocketTimeoutException

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.element
import scalaj.http.Http

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success}

object Util {
  type mMap[A,B] = mutable.HashMap[A,B]

  val cache: mMap[String, AnimeStream] =
    util.Try(decode[mMap[String, AnimeStream]](Source.fromFile("./items.json").getLines.mkString("\n")).right.get) match {
      case Success(v) => v
      case Failure(_) =>
        write("./items.json", "{}")
        mutable.HashMap[String,AnimeStream]()
    }

  type nmMap[A,B,C] = mMap[A, mMap[B, C]]

  val httpCache: nmMap[Int, String, String] =
    util.Try(decode[nmMap[Int, String, String]](Source.fromFile("./items.data").getLines.mkString).right.get) match {
      case Success(v) => v
      case Failure(_) =>
        write("./items.data", "{}")
        mutable.HashMap[Int, mutable.HashMap[String,String]]()
    }

  def write(file: String, data: String): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(file))
    pw.write(data)
    pw.close()
  }

  def write(file: String, data: Array[Byte]): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(file))
    pw.write(data.map(_.toChar))
    pw.close()
  }

  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = {
      write("./items.json", cache.asJson.spaces4)
      write("./items.data", httpCache.asJson.noSpaces)
    }
  })

  def host(a: String) = s"https://gogoanime.se$a"

  def http(num: Int, url: String) = {
    if (httpCache(num).contains(url)) httpCache(num)(url)
    else {
      val body = Http(url).asString.body
      httpCache(num).put(url, body)
      body
    }
  }

  def httpReq(num: Int, url: String): Browser#DocumentType = util.Try{
    JsoupBrowser().parseString(Util.http(num, url))
  } match {
    case Success(v) => v
    case Failure(e) => e match {
      case _: SocketTimeoutException => httpReq(num, url)
    }
  }

  def hasEps(doc: Browser#DocumentType) =
    util.Try((doc >> element("#episode_page > li > a")).attr("ep_end")) match {
      case Success(_) => true
      case Failure(_) => false
    }

  def url(animeMeta: AnimeMeta) =
    s"https://gogoanime.se/load-list-episode?ep_start=${animeMeta.start}&ep_end=${animeMeta.end}&id=${animeMeta.id}"
}
