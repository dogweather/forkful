---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML-tulkitseminen on prosessi, jolla sivustojen koodi muunnetaan strukturoiduksi datarakenteeksi. Ohjelmoijat tekevät sen usein datan poimimiseksi verkkosivuilta tai sivuston rakenteen ymmärtämiseksi.

## Näin teet:

Kotlinissa voit käyttää esimerkiksi jsoup-kirjastoa tähän tarkoitukseen. Katso alla olevan koodiesimerkin:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val doc = Jsoup.connect("https://example.com").get()
    val title = doc.title()
    println("Otsikko : $title")

    val links = doc.select("a[href]")
    for (link in links) {
        println("Linkki : ${link.attr("abs:href")}")
    }
}
```

Koodi käy läpi 'example.com' -sivuston, tulostaa sen otsikon ja kaikki linkit.

## Syvemmälle

HTML:n tulkitseminen sai alkunsa 1990-luvulla, kun verkkosivustot olivat tekstimuotoisia ja niiden kaivaminen oli välttämätöntä tiedonsaannin kannalta. Alternatiivit HTML-tulkitsemiselle sisältävät XML- tai JSON-tulkitsemisen, riippuen tiedon esittämisformaatin ja tarpeiden mukaan.

Tulkkauskirjastoissa, kuten Jsoup, on erityisen huolellinen tapa käsitellä erilaisia HTML-tageja ja niiden attribuutteja. Jsoup myös käsittelee poikkeukset hyvin, jotka saattavat syntyä huonosti muotoillun HTML:n kanssa.

## Katso myös

- Jsoup dokumentaatio: https://jsoup.org/
- Jsoup GitHub page: https://github.com/jhy/jsoup
- XML parsing Kotlinissa: https://kotlinlang.org/docs/xml.html
- JSON parsing Kotlinissa: https://kotlinlang.org/docs/kotlinx-serialization.html