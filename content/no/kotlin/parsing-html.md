---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse HTML handler om å analysere HTML-koden i en webside for å trekke ut relevant informasjon. Programvareutviklere gjør dette for å skrape data, automatisere webaktiviteter, eller analysere strukturen på en webside.

## Hvordan:
Her er et grunnleggende eksempel på HTML-parsing i Kotlin ved å bruke Jsoup biblioteket:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Tittelen på siden min</title></head></html>"
    val doc = Jsoup.parse(html)

    val title = doc.select("title").first().text()
    println(title)  // Skriver ut: "Tittelen på siden min"
}
```
I denne koden henter vi tittelen fra HTML-koden ved hjelp av Jsoup's `select` funksjonen.

## Dypdykk:

Historisk sett har teknikker for parsing av HTML utviklet seg sammen med språkets utvikling. Før Jsoup og lignende biblioteker, kunne HTML-parsing være en frustrerende oppgave som krevde komplekse regulære uttrykk eller manuell strengmanipulasjon.

Alternativene til Jsoup for HTML-parsing i Kotlin er biblioteker som HtmlUnit, Selenium og Jtidy. Valget mellom dem kan avhenge av mange faktorer, som graden av kompleksitet i HTML-dokumentene man arbeider med, eller behovet for å etterligne brukerinteraksjon.

Implementation-wise, Jsoup fungerer ved å oversette HTML til en intern datastruktur kjent som Document Object Model (DOM). Denne strukturen lar deg navigere og manipulere HTML som om det var en trestruktur.

## Se Også:
- [Jsoup Hjemmeside](https://jsoup.org/)
- [Kotlin Dokumentasjon](https://kotlinlang.org/docs/home.html)
- [Alternativ til Jsoup](https://www.baeldung.com/java-html-parsing)