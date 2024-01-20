---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Sende eine HTTP-Anforderung in Swift

## Was & Warum?

Ein HTTP-Anforderung senden bedeutet, mit Webservern zu kommunizieren und Ressourcen (wie Webseiten oder API-Daten) anzufordern. Als Programmierer tun wir dies, um mit externen Servern zu kommunizieren und so dynamischen Inhalt oder Webservices zu nutzen.

## How to:
Hier ist ein einfaches Beispiel, wie man eine HTTP GET-Anforderung in Swift sendet:

```Swift
import Foundation

let url = URL(string: "http://DeineWebseite.de/")!
let task = URLSession.shared.dataTask(with: url) {(data, response, error) in
    if let data = data {
        print(String(data: data, encoding: .utf8)!)
    }
}
task.resume()
```

Die Antwort von diesem Code wäre der HTML-Inhalt der angegebener Webseite.

## Deep Dive

HTTP steht für "HyperText Transfer Protocol", und es wurde ursprünglich 1991 eingeführt. Es ist das Protokoll, das vom World Wide Web zur Kommunikation genutzt wird. Alternativen zum Senden von HTTP-Anforderungen können Websockets oder GraphQL sein, die beide andere Methoden zum Abrufen von Daten von einem Server bieten.

In Swift sind `URLSession` und `URLRequest` die wichtigsten Klassen, die wir zum Senden von HTTP-Anforderungen nutzen. Indem wir eine `URL` an eine `URLSession` übergeben, können wir eine `URLSessionDataTask` erstellen, die dann mit `.resume()` gestartet werden kann.

## Siehe auch

Weitere Informationen zum Senden von HTTP-Anforderungen in Swift finden Sie in der [Apple-networking Dokumentation](https://developer.apple.com/documentation/foundation/url_loading_system) und auf [SwiftAPI](https://swiftapi.dev).

---
Hinweis: Denken Sie daran, den Netzwerkcode immer auf einem Hintergrundthread und nicht auf dem Hauptthread auszuführen. Islands Vorsicht ist die Mutter der Sicherheit!