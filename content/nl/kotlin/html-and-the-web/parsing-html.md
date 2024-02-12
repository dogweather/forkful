---
title:                "HTML Parsen"
aliases:
- /nl/kotlin/parsing-html.md
date:                  2024-01-28T22:03:32.645612-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML Parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
HTML parseren betekent het ontleden van de opmaak van een webpagina in iets dat een programma kan begrijpen en manipuleren. Programmeurs parseren HTML om gegevens te extraheren, webinteracties te automatiseren of inhoud tussen systemen te migreren.

## Hoe:
Kotlin maakt het parseren van HTML eenvoudig met bibliotheken zoals Jsoup. Zo doe je het:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Voorbeeld Pagina</title></head><body><p>Dit is een test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val titel = doc.title()
    println("Titel: $titel")  // Uitvoer: Titel: Voorbeeld Pagina

    val pTekst = doc.select("p").first()?.text()
    println("Paragraaf: $pTekst")  // Uitvoer: Paragraaf: Dit is een test.
}
```

We pakken de titel en paragraaftext, dat is slechts het begin van wat Jsoup kan doen. Maar het is een start.

## Diepere Duik:
Voor Kotlin was Java de standaard hiervoor, vaak op een onhandige manier. Jsoup veranderde het spel door een jQuery-achtige benadering te bieden. Het parseren van HTML is echter niet exclusief voor Jsoup; andere bibliotheken zoals HtmlUnit of zelfs regex (hoewel afgeraden) bestaan ook. Met Jsoup zorg je ervoor dat je parsing de structuur van het document respecteert. Het gebruikt een DOM-model, waardoor selectie en manipulatie van elementen mogelijk is. Het is ook veerkrachtig - het kan zelfs de rommeligste HTML parseren.

## Zie Ook:
Duik dieper in Jsoup:

- Jsoup officiële documentatie: https://jsoup.org/
- "Kotlin voor Android-ontwikkelaars" boek: https://antonioleiva.com/kotlin-android-developers-book/
- Officiële site van de Kotlin-programmeertaal: https://kotlinlang.org/

Voor bredere discussies en tutorials over webscraping en parsing:

- Web Scraping met Kotlin en Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- HTML parseren op Android met Kotlin en Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
