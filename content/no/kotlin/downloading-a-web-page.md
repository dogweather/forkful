---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å laste ned en webside innebærer å hente hele innholdet på en gitt URL slik at det kan brukes eller lagres lokalt. Programmerere gjør dette for å hente rå data, gjøre skraping, søke i innholdet på websider, lage offline kopier av nettsteder, eller som en del av integrasjonsflyt mellom applikasjoner.

## Hvordan:

```Kotlin
import java.net.URL

fun main() {
    val url = URL("http://exemple.com")
    val content = url.readText()
    println(content)
}
```
Dette lille programmet skrives til kommandolinjen og skriver ut innholdet på websiden "http://exemple.com" til kommandolinjegrensesnittet (CLI).

## Dypdykk

Historisk sett har henting av webinnhold vært en standard del av webutvikling siden dens tidlige dager. I Kotlin kan dette gjøres enkelt ved hjelp av `java.net.URL` klassen som standard i Java-biblioteket, som Kotlin kan bruke direkte.

Det er også flere alternativer for mer spesialiserte oppgaver, deriblant Jsoup for webscraping og oppløsning av DOM, eller okHttp for mer avansert nettverkshåndtering og håndtering av HTTP/HTTPS. 

Ved implementering er det viktig å merke seg at `readText()`-funksjonen leser hele webinnholdet til minnet før det returnerer som en streng. Dette er ok for mindre nettsider, men kan forårsake hukommelsesproblemer for større nettsider. I slike tilfeller vil buffering og strømbehandling være nødvendig.

## Se også

* [Java URL API Documentation](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/net/URL.html)
* [Jsoup Official Website](https://jsoup.org/)
* [OkHttp Official Website](https://square.github.io/okhttp/)
* [Kotlin Official Documentation](https://kotlinlang.org/docs/reference/)