---
title:                "Analyse av HTML"
date:                  2024-01-20T15:32:48.630276-07:00
html_title:           "Arduino: Analyse av HTML"
simple_title:         "Analyse av HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av HTML betyr å analysere og forstå innholdet i en HTML-fil slik at man kan manipulere eller hente spesifikk informasjon fra den. Programmerere parser HTML for å automatisere datainnsamling, sjekke nettsiders innhold, eller for å integrere tredjepartstjenester som skraper nettdata.

## Hvordan:

Kotlin er flott for å parse HTML siden det kan integrere Java-biblioteker som Jsoup. Her er et eksempel:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Hei Norge!</title></head><body><p>Dette er en HTML-tekst til parsing.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Tittelen på siden er: $title")

    val paragraph = doc.select("p").first().text()
    println("Innholdet i <p>-taggen er: $paragraph")
}

// Utdata:
// Tittelen på siden er: Hei Norge!
// Innholdet i <p>-taggen er: Dette er en HTML-tekst til parsing.
```

## Dypdykk:

Historisk har parsing av HTML vært gjort gjennom regelmessige uttrykk og manuell tekstanalyse, men det er komplekst og feilutsatt. Jsoup-biblioteket, en Java-bibliotek omskrevet for Kotlin-bruk, gir et mer robust og enklere alternativ. 

Metoder som `parse` lar deg konstruere et `Document`-objekt som representerer HTML-strukturen. Fra der, kan du bruke DOM-metoder som `select` og `text` for å hente ut og manipulere dataen.

Alternativer til Jsoup inkluderer HTML-parsers som er innebygd i Android SDK og andre biblioteker som HtmlCleaner eller tagsoup. Ytelsen og funksjonene vil variere, men Jsoup er kjent for sin brukervennlighet og ekstensiv støtte av CSS-selektorer.

## Se Også:

- Jsoup offisielle nettsted: https://jsoup.org/
- Kotlin dokumentasjon: https://kotlinlang.org/docs/reference/
- Guide til web scraping med Kotlin og Jsoup: https://medium.com/@hussien89aa/kotlin-and-jsoup-the-best-web-scraping-combo-5d63300e4f4a
- Android's offisiell guide til HTML: https://developer.android.com/guide/topics/text/html_compat