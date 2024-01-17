---
title:                "Das Versenden einer http-Anfrage."
html_title:           "Kotlin: Das Versenden einer http-Anfrage."
simple_title:         "Das Versenden einer http-Anfrage."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Senden einer HTTP-Anfrage ist ein wichtiger Prozess in der Webentwicklung, der es Programmierern ermöglicht, Daten von einer Website zu empfangen. Durch das Senden von HTTP-Anfragen können Programmierer auf Ressourcen wie Webseiten, Bilder oder Dateien zugreifen. Es ist ein unverzichtbares Werkzeug, um dynamische Inhalte auf Websites zu erstellen.

Wie geht's?
Kotlin hat eine integrierte Bibliothek zur Verarbeitung von HTTP-Anfragen, die sehr einfach zu bedienen ist. Zuerst müssen Sie die Bibliothek importieren, indem Sie ```import java.net.URL``` eingeben. Dann können Sie eine URL-Verbindung erstellen und eine Anfrage senden, indem Sie die Methode ```openConnection()``` aufrufen. Sie können die Verbindung konfigurieren und bestimmte Parameter setzen, bevor Sie die Anfrage senden. Hier ist ein Beispiel für das Senden einer einfachen HTTPS-Anfrage und das Empfangen der Antwort:

```Kotlin
import java.net.URL

val url = URL("https://example.com")
val connection = url.openConnection()
connection.requestMethod = "GET"
val responseCode = connection.responseCode
println("Response code: $responseCode")
```

Tiefentauchen
Das Senden von HTTP-Anfragen ist seit den Anfängen des World Wide Web ein grundlegender Prozess. Es ermöglichte die Zusammenarbeit von Websites und die Übertragung von Informationen zwischen ihnen. Obwohl Kotlin eine integrierte Bibliothek zur Verarbeitung von HTTP-Anfragen hat, gibt es auch andere Bibliotheken und Frameworks wie Retrofit und OkHttp, die bei der Verarbeitung von HTTP-Anfragen in komplexen Webanwendungen helfen können.

Schau dir auch an
- Offizielle Dokumentation von Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.net.-u-r-l.-open-connection.html
- Retrofit: https://square.github.io/retrofit/
- OkHttp: https://square.github.io/okhttp/