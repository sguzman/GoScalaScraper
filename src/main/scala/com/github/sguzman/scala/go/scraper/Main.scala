package com.github.sguzman.scala.go.scraper

import java.net.SocketTimeoutException

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import scalaj.http.Http

import scala.collection.mutable
import scala.io.Source
import scala.util.{Failure, Success}

object Main {
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
    JsoupBrowser().parseString(http(num, url))
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

  def main(args: Array[String]): Unit = {
    val pages = 1 to 40
    pages.foreach{i =>
      httpCache.put(i, mutable.HashMap())

      val anime = List(i)
        .par
        .flatMap{a =>
          List(a)
            .map(_.toString)
            .map(b => s"https://gogoanime.se/anime-list.html?page=$b")
            .map(b => httpReq(i, b))
            .flatMap(_ >> elementList("div.anime_list_body > ul > li > a"))
            .map(_.attr("href"))
        }
        .filter(a => !cache.contains(a))

      println(anime.toList.asJson.spaces4)

      val meta = anime
        .flatMap{a =>
          List(a)
            .map(host)
            .map(b => httpReq(i, b))
            .filter(hasEps)
            .map(b => AnimeMeta(a,
              b.>>(element("div.anime_info_body_bg > h1")).text,
              b.>>(element("div.anime_info_body_bg > img")).attr("src"),
              b.>>(element("#movie_id")).attr("value").toInt,
              b.>>(element("#episode_page > li > a")).attr("ep_start").toInt,
              b.>>(element("#episode_page > li > a")).attr("ep_end")
            ))
        }

      if (meta.nonEmpty) {
        println(meta.toList.asJson.spaces4)

        val eps = meta
          .flatMap{a =>
            List(a)
              .map(url)
              .map(b => httpReq(i, b))
              .map(b => b.>>(elementList("a[href]")))
              .map(b => b.map(_.attr("href").trim))
              .map(b => AnimeEps(a, b.reverse))
          }

        println(eps.toList.asJson.spaces4)

        val rawVidStream = eps
          .map{a =>
            AnimeHash(a, a.eps
              .map(b => s"https://gogoanime.se$b")
              .map(b => httpReq(i, b))
              .map(b => b.>>(element("div.download-anime > a[href]")))
              .map(_.attr("href"))
              .map(_.trim)
            )
          }

        println(rawVidStream.toList.asJson.spaces4)

        val vids = rawVidStream
          .map{a =>
            val stream = AnimeStream(a, a.vids
              .map(b => httpReq(i, b))
              .map(b => util.Try(b.>>(element("div.dowload > a[href]"))) match {
                case Success(v) => Some(v)
                case Failure(_) => None
              })
              .map(b => if (b.isDefined) b.get.attr("href") else "")
            )

            cache.put(stream.animeHash.animeEps.anime.animeUrl, stream)
            println(stream.asJson.spaces4)
            stream
          }

        write("./items.json", cache.asJson.spaces4)
        httpCache.remove(0)
        println(vids.toList.asJson.spaces4)
      }
    }
  }
}
