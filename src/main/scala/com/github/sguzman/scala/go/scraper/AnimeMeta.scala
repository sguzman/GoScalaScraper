package com.github.sguzman.scala.go.scraper

case class AnimeMeta(
  animeUrl: String,
  title: String,
  img: String,
  added: Long,
  `type`: String,
  summary: String,
  genres: List[String],
  release: String,
  status: String,
  id: Int,
  start: Int,
  end: String
)
