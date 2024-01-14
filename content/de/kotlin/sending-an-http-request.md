---
title:                "Kotlin: Senden einer HTTP-Anfrage"
simple_title:         "Senden einer HTTP-Anfrage"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Warum

Beim Programmieren in Kotlin ist es oft notwendig, HTTP-Anfragen zu senden. Dies kann zum Beispiel erforderlich sein, um Daten von einer API abzurufen oder um Daten auf einen Server hochzuladen. In diesem Blogbeitrag zeige ich Ihnen, wie Sie mit Kotlin HTTP-Anfragen senden können.

## Wie funktioniert es?

Um HTTP-Anfragen in Kotlin zu senden, benötigen Sie die Klasse `URL` aus der Standardbibliothek von Kotlin. Mit dieser Klasse können Sie eine URL-Verbindung zu einer bestimmten Internetadresse herstellen. Hier ist ein Beispiel:

```Kotlin

val url = URL("https://www.example.com/api/data")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
val responseCode = connection.responseCode

println("Response Code: $responseCode")

```

In diesem Beispiel erstellen wir eine URL-Verbindung zu "https://www.example.com/api/data" und senden dann eine GET-Anfrage. Die Antwort wird in Form eines Response Codes ausgegeben. Beachten Sie, dass die `requestMethod` auf "GET" gesetzt wurde, aber je nach Anwendungsfall können Sie auch andere Methoden wie POST, PUT oder DELETE verwenden.

Um Daten mit einem HTTP-POST-Request zu senden, können Sie die `OutputStream` der URL-Verbindung verwenden. Hier ist ein Beispiel:

```Kotlin

val url = URL("https://www.example.com/api/upload")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "POST"
connection.doOutput = true

val outputStream = DataOutputStream(connection.outputStream)
outputStream.writeBytes("Hello from Kotlin!")
outputStream.flush()
outputStream.close()

val responseCode = connection.responseCode
println("Response Code: $responseCode")

```

Hier senden wir eine POST-Anfrage an die Adresse "https://www.example.com/api/upload" und übergeben die Daten "Hello from Kotlin!". Beachten Sie, dass wir `doOutput` auf `true` setzen müssen, um die `OutputStream` zu aktivieren. Am Ende erhalten wir wieder einen Response Code.

## Deep Dive

Beim Senden von HTTP-Anfragen gibt es viele weitere Details zu beachten, wie z.B. der Umgang mit Cookies oder das Hinzufügen von HTTP-Headern. Hier sind einige hilfreiche Links, die Ihnen dabei helfen können, tiefer in das Thema einzusteigen:

- [Offizielle Dokumentation von Kotlin über das Erstellen von HTTP-Anfragen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.net.-u-r-l/-u-r-l-/open-connection.html)
- [Tutorial von Baeldung über das Senden und Empfangen von HTTP-Anfragen in Kotlin](https://www.baeldung.com/kotlin-http-request)
- [Ein Beitrag von JournalDev über das Arbeiten mit HTTPURLConnection in Kotlin](https://www.journaldev.com/2335/java-httpurlconnection-example-java-http-request-get-post)
- [Dokumentation von Oracle über das Senden von HTTP-Anfragen mit Java](https://docs.oracle.com/javase/tutorial/networking/urls/sending.html)

## Siehe auch

- [Eine Einführung in die Programmierung mit Kotlin](https://www.guru99.com/kotlin-tutorial.html)
- [Offizielle Webseite von Kotlin](https://kotlinlang.org/)
- [Kotlin-Kurse auf Udemy](https://www.udemy.com/topic/kotlin/)