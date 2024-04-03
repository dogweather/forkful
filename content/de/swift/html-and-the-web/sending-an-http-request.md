---
date: 2024-01-20 18:00:36.171704-07:00
description: 'How to: Swift macht es mit der URLSession-API einfach, HTTP-Requests
  zu starten. Hier ist ein minimalistischer GET-Request.'
lastmod: '2024-03-13T22:44:54.222349-06:00'
model: gpt-4-1106-preview
summary: Swift macht es mit der URLSession-API einfach, HTTP-Requests zu starten.
title: Einen HTTP-Request senden
weight: 44
---

## How to:
Swift macht es mit der URLSession-API einfach, HTTP-Requests zu starten. Hier ist ein minimalistischer GET-Request:

```Swift
import Foundation

let url = URL(string: "https://api.example.com/data")!
let task = URLSession.shared.dataTask(with: url) { data, response, error in
    if let error = error {
        print("Fehler: \(error)")
    } else if let data = data, 
              let dataString = String(data: data, encoding: .utf8) {
        print("Erhaltene Daten: \(dataString)")
    }
}
task.resume()
```

Wenn alles klappt, solltest du eine Antwort wie "Erhaltene Daten: ..." sehen.

## Deep Dive
HTTP-Anfragen sind seit den Anfängen des Web zentral für die Interaktion mit Servern. Alternativen wie WebSockets existieren für verschiedene Anwendungsfälle. In Swift ist URLSession die primäre Klasse für die Netzwerkkommunikation. Es kümmert sich um die Details der Netzwerkprotokolle und der Session-Verwaltung. Du kannst verschiedene Konfigurationen nutzen, um das Verhalten deiner Requests anzupassen, und CompletionHandler verwenden, um mit den asynchronen Antworten zu arbeiten.

## See Also
- Die offizielle Dokumentation zum URLSession: [URLSession](https://developer.apple.com/documentation/foundation/urlsession)
- Swift Evolution Proposal für die asynchrone Unterstützung in URLSession: [SE-0296](https://github.com/apple/swift-evolution/blob/main/proposals/0296-async-await.md)
- Ein tieferer Tauchgang in HTTP in Swift von Swift by Sundell: [Networking](https://www.swiftbysundell.com/basics/networking/)
