package com.github.sguzman.scala.go.scraper

import io.circe.parser.decode
import io.circe.syntax._
import io.circe.generic.auto._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elementList}
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

  def getHttpCache[A <: Any](key: String, proc: Browser#DocumentType => String, dec: String => A, newKey: String => Unit = println) = {
    val doc = JsoupBrowser().parseString(httpCache(key))

    if (!cache.contains(key)) {
      val value = proc(doc)
      cache.put(key, value)
      newKey(value)
    }

    getItemCache(key, dec)
  }

  def get[A](url: String, proc: Browser#DocumentType => String, dec: String => A, newKey: String => Unit = println) = {
    def retry: String = util.Try(Http(url).asString) match {
      case Success(v) => v.body
      case Failure(_) => retry
    }
    httpCache.put(url, retry)
    getHttpCache(url, proc, dec)
  }

  def cascade[A](url: String, proc: Browser#DocumentType => String, dec: String => A, newKey: String => Unit = println) = {
    if (cache.contains(url)) getItemCache(url, dec)
    else if (httpCache.contains(url)) getHttpCache(url, proc, dec)
    else get(url, proc, dec)
  }

  def main(args: Array[String]): Unit = {
    val pages = 1 to 40
    val animes = pages.par.flatMap{a =>
      type Ret = List[String]

      val proc: Browser#DocumentType => String =
        doc => doc.>>(elementList(".anime_list_body > .listing > li > a[href]")).map(_.attr("href")).asJson.toString
      val dec: String => Ret = s => decode[Ret](s).right.get
      cascade(s"https://gogoanime.se/anime-list.html?page=$a", proc, dec)
    }

    println(animes.toList.asJson.toString)

    final case class Anime(
                            title: String,
                            img: String,
                            `type`: String,
                            summary: String,
                            genres: List[String],
                            released: String,
                            status: String,
                            id: Int,
                            start: Option[String],
                            end: Option[String])

    val animePage = animes.map{a =>
      def proc(doc: Browser#DocumentType): String = {
        val types = doc.>>(elementList(".anime_info_body_bg > .type > span")).map(_.text.trim.stripSuffix(":"))
        val typesValues = doc.>>(elementList(".anime_info_body_bg > .type")).map(_.text)
        val textNodes = Map(types.zip(typesValues).map(a => a._1 -> a._2.stripPrefix(a._1).trim.stripPrefix(":").trim): _*)

        val ani = Anime(
          doc.>>(element(".anime_info_body_bg > h1")).text,
          doc.>>(element(".anime_info_body_bg > img")).attr("src"),
          textNodes("Type"),
          textNodes("Plot Summary").stripPrefix("Summary: ").trim(),
          textNodes("Genre").split(", ").toList,
          textNodes("Released"),
          textNodes("Status"),
          doc.>>(element("#movie_id[value]")).attr("value").toInt,
          for {
            start <- doc.>?>(element("#episode_page > li > a[ep_start]"))
          } yield start.text,
          for {
            end <- doc.>?>(element("#episode_page > li > a[ep_end]"))
          } yield end.text
        )

        ani.asJson.spaces4
      }

      def dec(s: String): Anime = decode[Anime](s).right.get
      cascade(s"https://gogoanime.se$a", proc, dec)
    }
  }
}
