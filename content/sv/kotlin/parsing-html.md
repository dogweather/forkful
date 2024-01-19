---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsa HTML handlar om att konvertera HTML-kod till strukturerade data. Programmerare gör detta för att enklare kunna extrahera, manipulera eller modifiera HTML-data.

## Så här gör du:
Kotlin har ett inbyggt bibliotek, Jsoup, speciellt utformat för HTML-parsing. Så här kan kod exempelvis se ut:

```Kotlin
val doc = Jsoup.connect("http://example.com").get()
val title = doc.title()

println("Rubrik: $title")
```

I detta exempel ansluter vi till en webbsida, hämtar dess HTML-kod och sparar den i `doc`. Därefter extraherar vi sidans titel och skriver ut den. Kom ihåg att importera Jsoup:
```Kotlin
import org.jsoup.Jsoup
```

## Djupgående
HTML-parsing har utvecklats över tid för att meet behovet av att läsa och förstå HTML-kod programmatiskt. Alternativ till Jsoup inkluderar htmlcleaner och jtidy, men Jsoup anses ofta vara mer modern och effektiv.

Detaljer som kan vara intressanta: Jsoup fungerar genom att genomsöka HTML-koden och bygga upp ett "DOM-träd"(Document Object Model) som representerar strukturen i HTML-dokumentet. Till detta träd kan du sedan utföra diverse operationer.

## Se även
För mer information om HTML-parsing kan du läsa följande resurser:

1. Jsoup's officiella dokumentation: [Klicka här](https://jsoup.org/)
2. Officiell Kotlin dokumentation: [Klicka här](https://kotlinlang.org/docs/home.html)
3. Kurs i HTML-parsing: [Klicka här](https://www.coursera.org/learn/data-structures-optimizing-performance)