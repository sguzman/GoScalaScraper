package com.github.sguzman.scala.go.scraper

import io.circe.parser.decode
import io.circe.syntax._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.elementList
import scalaj.http.Http

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Main {
  type map = mutable.HashMap[String, String]

  val cache: map =
    Try(decode[map](Source.fromFile("./items.json").getLines.mkString("\n")).right.get) match {
      case Success(v) => v
      case Failure(_) =>
        write("./items.json", "{}")
        mutable.HashMap()
    }

  val httpCache: map =
    Try(decode[map](Source.fromFile("./items.data").getLines.mkString).right.get) match {
      case Success(v) => v
      case Failure(_) =>
        write("./items.data", "{}")
        mutable.HashMap()
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

  def getItemCache[A](key: String, dec: String => A) = {
    dec(cache(key))
  }

  def getHttpCache[A <: Any](key: String, proc: Browser#DocumentType => String, dec: String => A) = {
    val doc = JsoupBrowser().parseString(httpCache(key))

    cache.put(key, proc(doc))
    getItemCache(key, dec)
  }

  def get[A](key: String, proc: Browser#DocumentType => String, dec: String => A) = {
    httpCache.put(key, Http(key).asString.body)
    getHttpCache(key, proc, dec)
  }

  def cascade[A](url: String, proc: Browser#DocumentType => String, dec: String => A) = {
    if (cache.contains(url)) getItemCache(url, dec)
    else if (httpCache.contains(url)) getHttpCache(url, proc, dec)
    else get(url, proc, dec)
  }

  def main(args: Array[String]): Unit = {
    val s = ""
    val doc = JsoupBrowser().parseString(s)
    val pages = 1 to 40
    val animes = pages.par.flatMap{a =>
      type Ret = List[String]

      val proc: Browser#DocumentType => String =
        doc => doc.>>(elementList(".anime_list_body > .listing > li > a[href]")).map(_.attr("href")).asJson.toString
      val dec: String => Ret = s => decode[Ret](s).right.get
      cascade(s"https://gogoanime.se/anime-list.html?page=$a", proc, dec)
    }

    println(animes)
  }
}
