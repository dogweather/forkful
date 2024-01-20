---
title:                "HTML parsen"
html_title:           "Arduino: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/parsing-html.md"
---

{{< edit_this_page >}}

# Artikel: HTML in Swift parsen

## Was & Warum?

HTML-Parsing bedeutet, den Quellcode einer Webseite zu analysieren und zu verarbeiten. Dies ermöglicht uns, bestimmte Daten aus dem HTML-Dokument zu extrahieren, wie z.B. Text, Links oder Bilder.

## So geht's:

Mit Swift 5.5 können wir die `SwiftSoup` Bibliothek zur HTML-Analyse verwenden. Hier ist ein einfaches Beispiel:

```Swift
import SwiftSoup

let html = "<html><body><p>Hallo Welt!</p></body></html>"
do {
    let doc: Document = try SwiftSoup.parse(html)
    let text: String = try doc.body()!.text()
    print(text)
    // Ausgabe: Hallo Welt!
} catch Exception.Error(let type, let message) {
    print(message)
} catch {
    print("catch error")
}
```
In diesem Code-Parsing-HTML-String erstellen wir ein `Document` Objekt und extrahieren dann den Text aus dem HTML-Code.

## Tiefere Einblicke

Früher war HTML-Parsing eine aufwendige und fehleranfällige Aufgabe. Mit modernen Bibliotheken wie `SwiftSoup` ist es jetzt viel einfacher und sicherer. Alternativen zu `SwiftSoup` sind das `WKWebKit` Framework und `XMLParser` von Apple.

Beim Parsen von HTML geht es nicht nur um das Extrahieren von Daten. Es kann auch zum Manipulieren oder Bereinigen von HTML-Dokumenten verwendet werden.

## Siehe auch

Zur Weiterbildung schlage ich die folgenden Ressourcen vor:

- Offizielle Dokumentation zu `SwiftSoup` auf GitHub: [SwiftSoup](https://github.com/scinfu/SwiftSoup)
- Swift-Protokolldokumentation zu `XMLParser`: [XMLParser](https://developer.apple.com/documentation/foundation/xmlparser)
- Programmierleitfaden zu `WKWebKit` von Apple: [WKWebKit](https://developer.apple.com/documentation/webkit/wkwebview)

Für weitere Informationen und Tutorials empfehle ich auch, die [Stack Overflow](https://stackoverflow.com/questions/tagged/swift) Fragen zu `Swift` zu durchsuchen.