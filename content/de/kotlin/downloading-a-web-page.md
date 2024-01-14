---
title:                "Kotlin: Das Herunterladen einer Webseite"
simple_title:         "Das Herunterladen einer Webseite"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Warum
Das Herunterladen von Webseiten ist ein wichtiger Teil der Webentwicklung. Oftmals muss man Webseiten von einer externen Quelle laden, um sie in seine eigene Anwendung zu integrieren oder um damit weiterzuarbeiten. Mit Kotlin ist es möglich, dies effizient und elegant zu realisieren.

## Wie geht das?
Um eine Webseite in Kotlin herunterzuladen, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung der integrierten `URL` Klasse. Hier ein einfaches Beispiel:

```Kotlin
val url = URL("https://www.example.com")
val connection: HttpURLConnection = url.openConnection() as? HttpURLConnection
connection?.requestMethod = "GET"

val responseCode = connection?.responseCode
println("Response code is: " + responseCode)

val inputStream = connection?.inputStream
inputStream?.bufferedReader()?.use {
    it.lines().forEach { line ->
        println(line)
    }
}
```

In diesem Beispiel wird eine URL erstellt und eine Verbindung geöffnet. Anschließend wird die Anfrage als GET-Methode ausgeführt und die Antwort wird ausgegeben. Das Ergebnis ist der HTML Inhalt der Webseite, welcher hier einfach in der Konsole ausgegeben wird.

## Tiefergehende Informationen
Das Herunterladen von Webseiten kann jedoch auch komplexer sein, je nachdem welche Anforderungen man hat. Manchmal müssen zusätzliche Header in der Anfrage übermittelt werden oder man möchte die Antwort im JSON Format erhalten. Hier bietet Kotlin viele Möglichkeiten, wie zum Beispiel die Verwendung von Bibliotheken wie OkHttp oder AsyncHttpClient.

Wenn es darum geht, Webseiten automatisiert herunterzuladen, können auch Frameworks wie Selenium eingesetzt werden. Diese ermöglichen das simulierte Laden einer Webseite und bieten somit noch mehr Flexibilität und Kontrolle über den gesamten Prozess.

## Siehe auch
- [Kotlin-Website](https://kotlinlang.org/)
- [URL in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-u-r-l/index.html)
- [OkHttp Bibliothek](https://square.github.io/okhttp/)
- [AsyncHttpClient Bibliothek](https://github.com/AsyncHttpClient/async-http-client)
- [Selenium Framework](https://www.selenium.dev/)