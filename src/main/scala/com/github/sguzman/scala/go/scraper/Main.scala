package com.github.sguzman.scala.go.scraper

import java.net.SocketTimeoutException

import io.circe.syntax._
import io.circe.generic.auto._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import scalaj.http.{Http, HttpRequest, HttpResponse}

import scala.util.{Failure, Success}

object Main {
  def host(a: String) = s"https://gogoanime.se$a"

  def retry(http: HttpRequest): HttpResponse[String] = util.Try(http.asString) match {
    case Success(v) => v
    case Failure(e) => e match {
      case _: SocketTimeoutException => retry(http)
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
    val pages = 1 to 1
    val anime = pages
      .par
      .flatMap{a =>
        List(a)
          .map(_.toString)
          .map(a => s"https://gogoanime.se/anime-list.html?page=$a")
          .map(Http.apply)
          .map(retry)
          .map(_.body)
          .map(JsoupBrowser().parseString)
          .flatMap(_ >> elementList("div.anime_list_body > ul > li > a"))
          .map(_.attr("href"))
      }

    println(anime.toList.asJson)

    val meta = anime
        .flatMap{a =>
          List(a)
            .map(host)
            .map(Http.apply)
            .map(retry)
            .map(_.body)
            .map(JsoupBrowser().parseString)
            .filter(hasEps)
            .map(b => AnimeMeta(a,
              b.>>(element("div.anime_info_body_bg > h1")).text,
              b.>>(element("div.anime_info_body_bg > img")).attr("src"),
              b.>>(element("#movie_id")).attr("value").toInt,
              b.>>(element("#episode_page > li > a")).attr("ep_start").toInt,
              b.>>(element("#episode_page > li > a")).attr("ep_end").toInt
            ))
        }

    println(meta.toList.asJson)

    val eps = meta
      .flatMap{a =>
        List(a)
          .map(url)
          .map(Http.apply)
          .map(retry)
          .map(_.body)
          .map(JsoupBrowser().parseString)
          .map(b => b.>>(elementList("a[href]")))
          .map(b => b.map(_.attr("href").trim))
          .map(b => AnimeEps(a, b))
      }

    println(eps.toList.asJson)

    eps
      .flatMap{a =>
        List(a)
          .flatMap(_.eps)
          .map(b => s"https://gogoanime.se$b")
          .map(Http.apply)
          .map(retry)
          .map(_.body)
          .map(JsoupBrowser().parseString)
          .map(b => b.>>(element("div.download-anime > a[href]")))
          .map(_.attr("href"))
      }
      .foreach(println)
  }
}
