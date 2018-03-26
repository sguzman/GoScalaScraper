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
  def host(a: String) = s"https://gogoanime.se$a"

  def http(cache: mutable.HashMap[String, String], url: String) = {
    if (cache.contains(url)) cache(url)
    else {
      val body = Http(url).asString.body
      cache.put(url, body)
      body
    }
  }

  def httpReq(cache: mutable.HashMap[String, String], url: String): Browser#DocumentType = util.Try{
    JsoupBrowser().parseString(http(cache, url))
  } match {
    case Success(v) => v
    case Failure(e) => e match {
      case _: SocketTimeoutException => httpReq(cache, url)
    }
  }

  def hasEps(doc: Browser#DocumentType) =
    util.Try((doc >> element("#episode_page > li > a")).attr("ep_end")) match {
      case Success(_) => true
      case Failure(_) => false
    }

  def url(animeMeta: AnimeMeta) =
    s"https://gogoanime.se/load-list-episode?ep_start=${animeMeta.start}&ep_end=${animeMeta.end}&id=${animeMeta.id}"

  def write(file: String, data: String): Unit = {
    import java.io._
    val pw = new PrintWriter(new File(file))
    pw.write(data)
    pw.close()
  }

  val cache: mutable.HashMap[String, AnimeStream] =
    util.Try(decode[mutable.HashMap[String, AnimeStream]](Source.fromFile("./items.json").getLines.mkString("\n")).right.get) match {
      case Success(v) => v
      case Failure(_) =>
        write("./items.json", "{}")
        mutable.HashMap()
    }

  val httpCache: mutable.HashMap[String, String] =
    util.Try(decode[mutable.HashMap[String,String]](Source.fromFile("./items.data").getLines.mkString("\n")).right.get) match {
      case Success(v) => v
      case Failure(_) =>
        write("./items.data", "{}")
        mutable.HashMap()
    }

  Runtime.getRuntime.addShutdownHook(new Thread {
    override def run(): Unit = {
      write("./items.json", cache.asJson.spaces4)
      write("./items.data", httpCache.asJson.noSpaces)
    }
  })

  def main(args: Array[String]): Unit = {
    val pages = 1 to 1
    val anime = pages
      .par
      .flatMap{a =>
        List(a)
          .map(_.toString)
          .map(b => s"https://gogoanime.se/anime-list.html?page=$b")
          .map(b => httpReq(httpCache, b))
          .flatMap(_ >> elementList("div.anime_list_body > ul > li > a"))
          .map(_.attr("href"))
      }
      .filter(a => !cache.contains(a))

    println(anime.toList.asJson.spaces4)

    val meta = anime
        .flatMap{a =>
          List(a)
            .map(host)
            .map(b => httpReq(httpCache, b))
            .filter(hasEps)
            .map(b => AnimeMeta(a,
              b.>>(element("div.anime_info_body_bg > h1")).text,
              b.>>(element("div.anime_info_body_bg > img")).attr("src"),
              b.>>(element("#movie_id")).attr("value").toInt,
              b.>>(element("#episode_page > li > a")).attr("ep_start").toInt,
              b.>>(element("#episode_page > li > a")).attr("ep_end").toInt
            ))
        }

    println(meta.toList.asJson.spaces4)

    val eps = meta
      .flatMap{a =>
        List(a)
          .map(url)
          .map(b => httpReq(httpCache, b))
          .map(b => b.>>(elementList("a[href]")))
          .map(b => b.map(_.attr("href").trim))
          .map(b => AnimeEps(a, b.reverse))
      }

    println(eps.toList.asJson.spaces4)

    val rawVidStream = eps
      .map{a =>
        AnimeHash(a, a.eps
              .map(b => s"https://gogoanime.se$b")
              .map(b => httpReq(httpCache, b))
              .map(b => b.>>(element("div.download-anime > a[href]")))
              .map(_.attr("href"))
              .map(_.trim)
        )
      }

    println(rawVidStream.toList.asJson.spaces4)

    val vids = rawVidStream
      .map{a =>
        val stream = AnimeStream(a, a.vids
          .map(b => httpReq(httpCache, b))
          .map(b => b.>>(element("div.dowload > a[href]")))
          .map(_.attr("href"))
        )

        cache.put(stream.animeHash.animeEps.anime.animeUrl, stream)
        println(stream.asJson.spaces4)
        stream
      }

    println(vids.toList.asJson.spaces4)
  }
}
