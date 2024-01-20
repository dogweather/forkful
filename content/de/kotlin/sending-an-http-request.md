---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Senden einer HTTP-Anfrage fragt ein Client-Computer (oder Browser) Informationen oder Dienste von einem Server an. Programmierer tun dies häufig, um API-Daten abzufragen, Seiten zu laden oder Daten an eine Datenbank zu senden.

## Wie man:

Mit Kotlin und der Ktor-Bibliothek sieht das Senden einer HTTP-Anfrage folgendermaßen aus:

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*

val client = HttpClient()
val httpResponse = client.get<String>("https://example.com")
client.close()
```

Hier ist die Ausgabe beispielhaft:

```Kotlin
"<!doctype html>\n<html>\n<head>\n    <title>Example Domain</title>\n   ...
```

## Tiefer Einblick:

HTTP-Anfragen begannen 1991 mit der Veröffentlichung des HTTP/0.9-Protokolls und haben sich seitdem stetig weiterentwickelt. Alternativ kann man HTTP-Anfragen in Kotlin auch mit Bibliotheken wie OkHttp oder Retrofit senden. Beim Senden einer HTTP-Anfrage mit Ktor öffnet der Client eine Verbindung zum gewünschten Server, sendet ein Anforderungspaket und wartet auf die Serverantwort, die zurückgeschickt wird und den angeforderten Inhalt oder Statuscodes enthält.

## Siehe auch:

- OkHttp: [https://square.github.io/okhttp](https://square.github.io/okhttp)
- Retrofit: [https://square.github.io/retrofit](https://square.github.io/retrofit)