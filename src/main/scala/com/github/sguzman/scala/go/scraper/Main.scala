package com.github.sguzman.scala.go.scraper

object Main {
  def main(args: Array[String]): Unit = {
    rx.Observable.range(1, 40)
      .map[String](_.toString)
      .subscribe(t => {
        println(t)
      })
  }
}
