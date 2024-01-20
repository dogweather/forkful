---
title:                "Eine Webseite herunterladen"
html_title:           "Arduino: Eine Webseite herunterladen"
simple_title:         "Eine Webseite herunterladen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Webseiten herunterladen in Kotlin

## Was & Warum?

Das Herunterladen einer Webseite ist der Prozess der Lokalisierung von Webinhalten auf Ihrem Gerät. Programmierer machen das zum Web-Scraping, Datenextraktion und für Tests.

## So geht's:

``` Kotlin
import java.io.InputStreamReader
import java.net.URL
import java.nio.charset.Charset

fun main() {
    val url = URL("http://example.com/") 

    val reader = InputStreamReader(url.openStream(), Charset.forName("UTF-8"))
    reader.use {
        println(it.readText())
    }
}
```

Führen Sie das obige Skript aus, und es wird den gesamten HTML-Inhalt von "http://example.com/" auf die Konsole drucken.

## Tiefgang:

Historisch gesehen wurde das Herunterladen von Seiten mit Sprachen wie Perl und Shell-Skripten gemacht. Kotlin, eine moderne, typsichere Programmiersprache von JetBrains, hat eine minimalistische Syntax, die das Schreiben und Lesen einfach macht.

Alternative Methoden zum Herunterladen von Webseiten umfassen das Nutzen von Bibliotheken wie Jsoup oder OkHttp, die auch Funktionen für die HTML-Analyse und Netzwerkanfragen bieten.

Bei der Implementierung beachten Sie, dass das ständige Herunterladen von Inhalten von einer Seite zu einer IP-Sperre führen kann. Außerdem beachten Sie die Regeln in der `robots.txt` einer Webseite und die gesetzlichen Bestimmungen hinsichtlich des Daten-Scrapings.

## Siehe Auch:

[Offizielle Kotlin-Dokumentation](https://kotlinlang.org/docs/home.html)

[Bibliothek Jsoup](https://jsoup.org/)

[Bibliothek OkHttp](https://square.github.io/okhttp/)