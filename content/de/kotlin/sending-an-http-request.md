---
title:                "Eine HTTP-Anfrage senden"
html_title:           "Kotlin: Eine HTTP-Anfrage senden"
simple_title:         "Eine HTTP-Anfrage senden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

HTTP-Anfragen sind ein grundlegender Bestandteil der Webentwicklung und ermöglichen es, Daten zwischen einem Client und einem Server auszutauschen. Mit Kotlin können Sie einfach und effizient HTTP-Anfragen in Ihrer Anwendung implementieren und somit die Funktionalität und Benutzererfahrung verbessern.

## Wie Funktioniert es

Um eine HTTP-Anfrage mit Kotlin zu senden, benötigen Sie zunächst eine URL, an die die Anfrage gesendet werden soll. Dann können Sie die Kotlin-Standardbibliothek verwenden, um eine Verbindung zu dieser URL herzustellen. Hier ist ein Beispiel für das Senden einer einfachen GET-Anfrage und das Ausgeben der empfangenen Daten:

```Kotlin
val url = URL("https://www.example.com")
val connection = url.openConnection()
val response = connection.getInputStream().reader().readText()
println(response)
```

Dieses Beispiel erstellt eine Verbindung zu der angegebenen URL und liest den Inhalt der Seite, die zurückgegeben wird. Sie können auch POST-Anfragen senden und Daten an den Server mitgeben. Hier ist ein Beispiel dafür:

```Kotlin
val url = URL("https://www.example.com")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "POST"
connection.doOutput = true
val postData = "key=value&anotherKey=anotherValue"
connection.outputStream.write(postData.toByteArray())
val response = connection.inputStream.reader().readText()
println(response)
```

In diesem Beispiel wird eine Verbindung zu der angegebenen URL hergestellt und eine POST-Anfrage mit den angegebenen Daten gesendet. Nachdem die Anfrage an den Server gesendet wurde, wird die empfangene Antwort ausgegeben. 

Diese Codebeispiele sind nur ein einfacher Einblick in die Verwendung von Kotlin für HTTP-Anfragen und es gibt viele weitere Möglichkeiten, je nach den Anforderungen Ihrer Anwendung.

## Tiefere Einblicke

Wenn Sie mehr über die Funktionsweise von HTTP-Anfragen erfahren möchten, können Sie sich mit verschiedenen HTTP-Methoden auseinandersetzen, z.B. GET, POST, PUT, DELETE usw. Sie können auch die Verwendung von Header-Informationen und die Verarbeitung von Anfrage- und Antwortdaten genauer untersuchen. 

Eine wichtige Sache, auf die Sie achten müssen, ist die Verwendung von Threads beim Senden von HTTP-Anfragen. Wenn Sie sicherstellen möchten, dass Ihre Anwendung während des Sendens von Anfragen nicht eingefroren wird, können Sie asynchrone Methoden wie `async` und `await` verwenden, um Ihre Anfragen im Hintergrund auszuführen.

## Siehe auch

- [Offizielle Kotlin-Dokumentation zu HTTP-Anfragen](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.net.-u-r-l-/-u-r-l-connection/index.html)
- [Ein ausführliches Tutorial über das Senden von HTTP-Anfragen mit Kotlin](https://developer.android.com/kotlin/more-apis/usage-of-http-apis)
- [Eine Übersicht über die verschiedenen HTTP-Methoden und ihre Verwendung](https://www.tutorialspoint.com/http/http_methods.htm)
- [Eine Anleitung zur Verwendung von Threads in Kotlin](https://kotlinlang.org/docs/reference/coroutines/basics.html)