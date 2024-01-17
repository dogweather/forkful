---
title:                "Das Senden einer http Anfrage"
html_title:           "Gleam: Das Senden einer http Anfrage"
simple_title:         "Das Senden einer http Anfrage"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Senden einer HTTP-Anfrage ist ein grundlegender Teil der Entwicklung von Webanwendungen. Es ist ein Prozess, bei dem eine Anfrage von einem Client an einen Server gesendet wird, um Daten oder Ressourcen auszutauschen. Programmierer nutzen dies, um mit verschiedenen APIs zu interagieren oder Daten von externen Servern abzurufen und in ihre Anwendungen zu integrieren.

## How to:
Um eine HTTP-Anfrage in Gleam zu senden, können wir das Modul [Httpc](https://gleam.run/modules/httpc.html) verwenden. Hier ist ein Beispiel, wie wir eine GET-Anfrage an die URL "https://example.com" senden können:

```Gleam
let url = "https://example.com"

let result = Httpc.get(url)

case result {
  Ok(response) -> Httpc.ResponseMessage {
    // Wenn die Anfrage erfolgreich ist, können wir auf die Antwort zugreifen
    response.status_code // => 200
    response.body // => <p>Beispiel</p>
  }
  Error(_) -> Httpc.Error {
    // Wenn ein Fehler auftritt, können wir darauf reagieren
    // oder eine Fehlermeldung zurückgeben
    "Fehler beim Senden der Anfrage"
  }
}
```

## Deep Dive

HTTP-Anfragen existieren seit Anfang der 1990er Jahre und sind ein grundlegender Bestandteil des World Wide Web. Sie sind eine der grundlegenden Funktionen, die es ermöglichen, dass Webseiten und Anwendungen Daten und Ressourcen von anderen Servern erhalten.

Es gibt auch alternative Methoden, um HTTP-Anfragen in Gleam zu senden, wie zum Beispiel das Paket [Http](https://github.com/gleam-lang/gleam/blob/main/lib/standard/library/http/http.gleam), das die HTTP-Client-Funktionalität aus der Standardbibliothek erweitert.

Bei der Implementierung einer HTTP-Anfrage ist es wichtig, die beste Methode für den jeweiligen Anwendungsfall zu wählen. Es ist auch wichtig, auf Sicherheitsaspekte wie die Verwendung von HTTPS-Verbindungen zu achten.

## Sieh auch

- [HTTP Basics](https://www.w3schools.com/whatis/whatis_http.asp) auf W3Schools
- [Einführung in HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview) auf MDN Web Docs.