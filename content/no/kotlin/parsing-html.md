---
title:                "Analysering av html"
html_title:           "Kotlin: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing HTML er prosessen med å analysere og tolke kildekoden til en nettside for å ekstrahere spesifikke deler av informasjonen, som for eksempel tekst eller bilder. Dette er nyttig for å automatisere oppgaver som å hente informasjon fra flere nettsider samtidig, eller for å lage egendefinerte nettsøk.

## Slik gjør du det:

Inkluder følgende kode i en Kotlin-fil for å importere biblioteket som trengs for å parse HTML:

```
import org.jsoup.Jsoup
```

Deretter bruker du følgende kode for å få tak i kildekoden til en nettside og ekstrahere informasjonen du ønsker:

```
val doc = Jsoup.connect("URL til nettsiden").get()
val element = doc.select("CSS-selector") // Dette velger elementet du ønsker å ekstrahere
println(element.text())
```

Denne koden vil gi deg tekstinnholdet til det valgte elementet. Det finnes også flere metoder som kan brukes til å hente ut andre typer informasjon, for eksempel attributter eller attributtverdier til et HTML-element.

## Dypdykk:

Parsing av HTML ble først introdusert i World Wide Web-prosjektet i 1993, og har blitt en viktig teknikk for web scraping og automatisering av weboppgaver. I tillegg til å bruke biblioteker som Jsoup, finnes det også andre alternativer som XML-parser for å parse HTML-dokumenter. Det finnes også mer avanserte teknikker som DOM (Document Object Model) og SAX (Simple API for XML) for å håndtere og manipulere HTML-strukturen.

## Se også:

- [The Evolution of HTML Parsing](https://stackoverflow.com/questions/1074431/history-of-html-parsing)
- [JSoup Documentation](https://jsoup.org/apidocs/)
- [Alternatives to JSoup](https://jsoup.org/alternatives)