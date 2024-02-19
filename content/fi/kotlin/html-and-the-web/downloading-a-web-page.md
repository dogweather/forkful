---
aliases:
- /fi/kotlin/downloading-a-web-page/
date: 2024-01-20 17:44:23.735953-07:00
description: "Verkkosivun lataaminen tarkoittaa sivun sis\xE4ll\xF6n hakemista internetist\xE4\
  \ omalle laitteelle. Koodarit lataavat sivuja data-analyysiin, sis\xE4ll\xF6n\u2026"
lastmod: 2024-02-18 23:09:07.573546
model: gpt-4-1106-preview
summary: "Verkkosivun lataaminen tarkoittaa sivun sis\xE4ll\xF6n hakemista internetist\xE4\
  \ omalle laitteelle. Koodarit lataavat sivuja data-analyysiin, sis\xE4ll\xF6n\u2026"
title: Verkkosivun lataaminen
---

{{< edit_this_page >}}

## What & Why?
Verkkosivun lataaminen tarkoittaa sivun sisällön hakemista internetistä omalle laitteelle. Koodarit lataavat sivuja data-analyysiin, sisällön varmuuskopiointiin, tai sovellusten rakentamiseen joka hyödyntää sivujen tietoja.

## How to:
Kotlinissa verkkosivun voi ladata käyttäen `URL`-luokkaa ja `readText`-metodia. Tässä helppo esimerkki:

```Kotlin
import java.net.URL

fun downloadWebPage(pageUrl: String): String {
    return URL(pageUrl).readText(Charsets.UTF_8)
}

fun main() {
    val webContent = downloadWebPage("http://example.com")
    println(webContent)
}
```

Jos kokeilet koodia ja sivusto on kunnossa, saat tulosteena sivun HTML-sisältöä.

## Deep Dive
Verkkosivun lataaminen on vanha käytäntö, joka alkoi, kun web ensi kerran keksittiin. Alkujaan tehdyt käsityönä, mutta nykyisin prosessit ovat automatisoituja ja integroituja.

Vaihtoehtoina yksinkertaiselle `URL`-luokan käytölle on erilaisia kirjastoja, kuten OkHttp ja kjsoup. OkHttp tarjoaa täyden HTTP-clientin, kun taas jsoup soveltuu HTML:n jäsentämiseen ja käsittelyyn.

`readText` toimii hyvin pienille sivuille, mutta suuriin datajoukkoihin tai monimutkaisiin pyyntöihin kannattaa käyttää välineitä, jotka käsittelevät virheet ja striimauksen paremmin.

## See Also
- OkHttp: https://square.github.io/okhttp/
- jsoup: https://jsoup.org/
- Kotlin Networking Tutorial: https://kotlinlang.org/docs/networking.html
