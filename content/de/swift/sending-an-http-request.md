---
title:                "Senden einer http-Anfrage"
html_title:           "Swift: Senden einer http-Anfrage"
simple_title:         "Senden einer http-Anfrage"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen ist eine häufige Aufgabe in der App-Entwicklung, besonders wenn man mit APIs arbeitet. Es ermöglicht die Kommunikation zwischen einer Anwendung und einem Server, um Daten zu senden und zu empfangen. Dies ist entscheidend, um dynamische Inhalte in Apps bereitzustellen und Benutzerinteraktionen zu ermöglichen.

## Wie geht's

Um eine HTTP-Anfrage in Swift zu senden, können wir die `URLSession` verwenden. Zuerst müssen wir eine URL erstellen, die die Adresse der Ressource enthält, auf die wir zugreifen möchten. Dann können wir mithilfe dieser URL eine `URLRequest` erstellen. Schließlich können wir die Anfrage mit `URLSession` ausführen und die Daten empfangen.

Ein Beispiel für das Senden einer GET-Anfrage:

```Swift
// URL erstellen
let url = URL(string: "https://example.com/api/data")
// URLRequest erstellen
let request = URLRequest(url: url!)
// URLSession verwenden, um die Anfrage auszuführen
let task = URLSession.shared.dataTask(with: request) { data, response, error in
    if let data = data {
        // Daten verarbeiten
    }
}
// Anfrage starten
task.resume()
```

Die Ausgabe der Anfrage wäre die empfangenen Daten, die wir dann verarbeiten können, je nachdem, welche Art von Daten wir erwarten.

## Tief eintauchen

`URLSession` bietet verschiedene Arten von Anfragen wie GET, POST, PUT, DELETE usw. an. Wir können auch zusätzliche Konfigurationsoptionen hinzufügen, indem wir `URLSessionConfiguration` verwenden. Zum Beispiel können wir die Timeout-Zeit, den Cache-Richtlinien und vieles mehr anpassen.

Eine wichtige Sache, auf die man achten sollte, ist das Behandeln von Fehlern. Beim Senden einer HTTP-Anfrage gibt es viele mögliche Fehler, wie zum Beispiel Netzwerkprobleme oder ungültige URLs. Es ist wichtig, diese Fehler zu erfassen und entsprechend zu behandeln, um sicherzustellen, dass unsere Anwendung zuverlässig funktioniert.

## Siehe auch

- [Apple Developer Dokumentation zu URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [HTTP-Methoden und REST](https://www.restapitutorial.com/lessons/httpmethods.html)
- [Swift und HTTP](https://www.swiftbysundell.com/basics/http-requests/)