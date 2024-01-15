---
title:                "Das Versenden einer http-Anfrage"
html_title:           "Gleam: Das Versenden einer http-Anfrage"
simple_title:         "Das Versenden einer http-Anfrage"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen Welt der ständig wachsenden Technologie ist es unerlässlich, mit verschiedenen Webanwendungen und APIs zu interagieren. Das Senden von HTTP-Anfragen ist ein wesentlicher Bestandteil dieser Interaktion und ermöglicht es, Daten aus dem Internet abzurufen oder zu senden.

## Wie funktioniert es?

Die Gleam-Programmiersprache bietet eine einfache und effektive Möglichkeit, HTTP-Anfragen zu senden. Hier ist ein Beispielcode, der eine GET-Anfrage an die GitHub API sendet und die Antwort verarbeitet:

```Gleam
import http

let url = "https://api.github.com/users/username"
let request = http.get(url)
let response = request.send()
let body = response.body() # Konvertiert die Antwort in einen String
let json = Json.parse(body) # Verarbeitet den String als JSON

let status = response.status() # Gibt den Statuscode zurück
let headers = response.headers() # Gibt eine Liste der Antwortheader zurück
let content_type = response.content_type() # Gibt den Inhaltstyp der Antwort zurück
```

In diesem Beispiel wird die Gleam-Bibliothek "http" importiert, um die HTTP-Anfrage zu erstellen und zu senden. Dann wird die Antwort verarbeitet, indem sie in einen String konvertiert und als JSON interpretiert wird. Schlussendlich können auch der Statuscode, die Antwortheader und der Inhaltstyp der Antwort ausgelesen werden.

## Tiefere Einblicke

Um eine vollständigere HTTP-Anfrage zu erstellen, können auch Optionen wie Header, Cookies und Timeout-Werte angegeben werden. Hier ist ein Beispiel, das den zuvor gezeigten Code erweitert:

```Gleam
import http

let url = "https://api.github.com/users/username"
let headers = [( "Authorization", "Token xxxxxxxx" )]
let cookies = [( "session", "123456789" )]
let request_options =
http.Options(headers=headers, cookies=cookies, timeout=1000)
let request = http.get(url, options=request_options)
let response = request.send()
```

In diesem Beispiel werden die Optionen "headers" und "cookies" gesetzt und an die HTTP-Anfrage übergeben. Außerdem wird ein Timeout-Wert von 1000 Millisekunden gesetzt, um die Anfrage abzufangen, falls die Serverantwort zu lange dauert.

## Siehe auch

- Offizielle Gleam-Dokumentation zu HTTP-Anfragen: https://gleam.run/stdlib/http.html
- Github-Repository mit Beispielen für HTTP-Anfragen in Gleam: https://github.com/gleam-lang/gleam_stdlib/tree/master/examples/http