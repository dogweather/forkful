---
title:                "HTML parsen"
date:                  2024-01-20T15:34:03.721547-07:00
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML bedeutet, den Inhalt und die Struktur von HTML-Code zu analysieren und zu verstehen. Programmierer machen das, um Daten aus Webseiten zu extrahieren oder deren Inhalt programmgesteuert zu verarbeiten.

## Vorgehensweise:
In Swift nutzen wir Libraries wie `SwiftSoup` für das Parsen von HTML. Hier ein kurzes Beispiel:

```Swift
import SwiftSoup

let html = "<html><head><title>Willkommen</title></head><body><p>Hallo Welt!</p></body></html>"
do {
    let doc = try SwiftSoup.parse(html)
    let bodyText = try doc.body()?.text()
    print(bodyText ?? "Kein Text gefunden.")
} catch Exception.Error(let type, let message) {
    print("Fehler \(type): \(message)")
} catch {
    print("Ein unbekannter Fehler ist aufgetreten.")
}
```

Ausgabe:
```
Hallo Welt!
```

## Vertiefung:
HTML-Parsing hat seine Wurzeln in der Notwendigkeit, Informationen aus dem Web zu extrahieren, die nicht direkt über APIs verfügbar sind. Im Laufe der Zeit entstanden verschiedene Libraries und Tools für verschiedene Programmiersprachen. Vor `SwiftSoup` gab es in der Swift-Welt nicht viele Optionen – Entwickler griffen oft auf Objective-C Libraries zurück oder nutzten `NSRegularExpression` für einfaches Parsing. `SwiftSoup` ist inspiriert von `JSoup`, einer populären Java-Library, und bietet eine komfortable und sichere Art, HTML in Swift zu handeln. Alternativ könnten Entwickler auch auf WebKit's `DOM` Parsing zurückgreifen, aber dies ist meist aufwendiger und weniger flexibel.

## Siehe auch:
- SwiftSoup GitHub: https://github.com/scinfu/SwiftSoup
- JSoup: https://jsoup.org – wenn du mehr über das Original erfahren möchtest
- Swift Dokumentation zu `NSRegularExpression`: https://developer.apple.com/documentation/foundation/nsregularexpression – falls du den klassischen Weg gehen möchtest
