package com.github.sguzman.scala.go.scraper

import io.circe.generic.auto._
import io.circe.syntax._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

import scala.collection.mutable
import scala.util.{Failure, Success}

object Main {
  def main(args: Array[String]): Unit = {
    val pages = 1 to 1
    pages.foreach{i =>
      Util.httpCache.put(i, mutable.HashMap())

      val anime = List(i)
        .par
        .flatMap{a =>
          List(a)
            .map(_.toString)
            .map(b => s"https://gogoanime.se/anime-list.html?page=$b")
            .map(b => Util.httpReq(i, b))
            .flatMap(_ >> elementList("div.anime_list_body > ul > li > a"))
            .map(_.attr("href"))
        }
        .filter(a => !Util.cache.contains(a))

      println(anime.toList.asJson.spaces4)

      val meta = anime
        .flatMap{a =>
          List(a)
            .map(Util.host)
            .map(b => Util.httpReq(i, b))
            .filter(Util.hasEps)
            .map{b =>
              val types = b.>>(elementList(".anime_info_body_bg > p.type > span")).map(_.text.trim.stripSuffix(":"))
              val typeValues = b.>>(elementList(".anime_info_body_bg > p.type")).zipWithIndex.map(c => c._1.text.stripPrefix(types(c._2) ++ ": "))
              val map = Map[String,String](types.zip(typeValues): _*)

              AnimeMeta(a,
                b.>>(element("div.anime_info_body_bg > h1")).text,
                b.>>(element("div.anime_info_body_bg > img")).attr("src"),
                java.time.Instant.now.getEpochSecond,
                map("Type"),
                map("Plot Summary"),
                map("Genre").split(", ").toList,
                map("Released"),
                map("Status"),
                b.>>(element("#movie_id")).attr("value").toInt,
                b.>>(element("#episode_page > li > a")).attr("ep_start").toInt,
                b.>>(element("#episode_page > li > a")).attr("ep_end")
              )
            }
        }

      if (meta.nonEmpty) {
        println(meta.toList.asJson.spaces4)

        val eps = meta
          .flatMap{a =>
            List(a)
              .map(Util.url)
              .map(b => Util.httpReq(i, b))
              .map(b => b.>>(elementList("a[href]")))
              .map(b => b.map(_.attr("href").trim))
              .map(b => AnimeEps(a, b.reverse))
          }

        println(eps.toList.asJson.spaces4)

        val vids = eps
          .map{a =>
            AnimeHash(a, a.eps
              .map(b => s"https://gogoanime.se$b")
              .map(b => Util.httpReq(i, b))
              .map(b => b.>>(element("div.download-anime > a[href]")))
              .map(_.attr("href"))
              .map(_.trim)
            )
          }

        println(vids.toList.asJson.spaces4)

        //TODO - fix video link parser

        Util.write("./items.json", Util.cache.asJson.spaces4)

        Util.httpCache(i).clear()
        Util.httpCache.put(i, mutable.HashMap())
        Util.httpCache.remove(i)

        println(vids.toList.asJson.spaces4)
      }
    }
  }
}
