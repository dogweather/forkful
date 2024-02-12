---
title:                "Verkkosivun lataaminen"
aliases: - /fi/kotlin/downloading-a-web-page.md
date:                  2024-01-20T17:44:23.735953-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/downloading-a-web-page.md"
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
