---
title:                "Swift: Senden einer http-Anfrage"
simple_title:         "Senden einer http-Anfrage"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Senden von HTTP-Anfragen beschäftigen? Ganz einfach: In der heutigen Zeit der Vernetzung und des Internets sind HTTP-Anfragen ein unverzichtbares Werkzeug für die Kommunikation zwischen verschiedenen Computern und Servern. Sie ermöglichen es uns, Daten aus dem Internet abzurufen und zu nutzen, sei es für die Anzeige von Webseiten oder für das Abrufen von Informationen in einer App.

## Wie geht das?

Das Senden von HTTP-Anfragen ist in Swift gar nicht so kompliziert, wie man vielleicht denken mag. Mit Hilfe der URLSession-Klasse können wir eine Verbindung zu einer URL aufbauen und eine Anfrage senden. Hier ein Beispiel:

```Swift
// Erstellen einer URL
let url = URL(string: "https://www.example.com")!

// Erstellen einer URLRequest
let request = URLRequest(url: url)

// Initialisierung einer URLSession
let session = URLSession(configuration: .default)

// Senden einer asynchronen HTTP-Anfrage
let task = session.dataTask(with: request) { data, response, error in
    // Hier können wir nun die Antwort auswerten
    if let data = data {
        // Wir können die Daten in einen String umwandeln
        let responseString = String(data: data, encoding: .utf8)
        print(responseString)
    }
}

// Starten der Anfrage
task.resume()
```

Dieses Beispiel sendet eine asynchrone HTTP-Anfrage an die URL "https://www.example.com" und gibt die empfangenen Daten (falls vorhanden) als String aus. Natürlich gibt es noch viele weitere Möglichkeiten und Optionen beim Senden von HTTP-Anfragen, aber das grundlegende Konzept bleibt dasselbe.

## Eine tiefere Analyse

Nun wollen wir etwas tiefer in die Materie eintauchen und uns genauer anschauen, wie HTTP-Anfragen funktionieren. Im Grunde genommen ist eine HTTP-Anfrage einfach eine Nachricht, die von einem Client (z.B. einer App) an einen Server gesendet wird. Diese Nachricht enthält Informationen über die gewünschte Aktion (z.B. Daten abrufen), den URL, und optional auch Daten, die an den Server übertragen werden sollen (z.B. in Form von JSON). Der Server antwortet dann mit einer Nachricht, die wiederum Informationen darüber enthält, ob die Anfrage erfolgreich war und ggf. weitere Daten oder Informationen.

## Siehe auch

- [Apple Developer Documentation: URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- [What is an HTTP Request?](https://www.lifewire.com/what-is-an-http-request-2625796)
- [Understanding HTTP Request-Response Messages](https://www.xml.com/pub/a/ws/2001/03/27/httpreq.html)