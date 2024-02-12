---
title:                "HTML parsen"
aliases: - /de/swift/parsing-html.md
date:                  2024-02-03T19:13:18.620679-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML parsen"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML bezieht sich auf den Prozess des Aufbrechens und Interpretierens der Struktur von HTML-Inhalten, typischerweise um spezifische Daten zu extrahieren oder diesen Inhalt programmatisch zu manipulieren. Programmierer beschäftigen sich mit HTML-Parsing für Web-Scraping, Data-Mining, automatisierte Tests und Inhaltsmigrationen, was Anwendungen ermöglicht, effizient mit Webdokumenten zu interagieren und sie zu verarbeiten.

## Wie:
Swift enthält standardmäßig keine integrierte Bibliothek für das HTML-Parsing, weshalb die Verwendung von Drittanbieter-Bibliotheken notwendig ist, um diese Aufgabe effektiv zu bewältigen. Eine der beliebtesten Optionen ist SwiftSoup, eine reine Swift-Bibliothek, die eine jQuery-ähnliche Syntax für das HTML-Parsing und die Manipulation bietet.

### Installation
Zuerst müssen Sie SwiftSoup zu Ihrem Projekt hinzufügen. Wenn Sie den Swift Package Manager verwenden, können Sie es zu Ihren Abhängigkeiten in `Package.swift` hinzufügen:

```swift
dependencies: [
    .package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.3.2")
]
```

### Beispiel: Extrahieren von Links aus HTML
Angenommen, Sie haben ein HTML-Dokument und möchten alle Links (`<a href="...">`) extrahieren. Mit SwiftSoup können Sie dies leicht bewerkstelligen:

```swift
import SwiftSoup

let html = """
<!DOCTYPE html>
<html>
<head>
    <title>Beispielseite</title>
</head>
<body>
    <p>Willkommen auf unserer Website</p>
    <a href="https://example.com/page1">Seite 1</a>
    <a href="https://example.com/page2">Seite 2</a>
</body>
</html>
"""

do {
    let doc: Document = try SwiftSoup.parse(html)
    let links: Elements = try doc.select("a")
    for link in links.array() {
        let linkHref: String = try link.attr("href")
        let linkText: String = try link.text()
        print("\(linkText) - \(linkHref)")
    }
} catch Exception.Error(let type, let message) {
    print("Fehlertyp: \(type) Nachricht: \(message)")
} catch {
    print("Fehler")
}
```

### Beispiel-Ausgabe
Der vorherige Code extrahiert URLs und deren Text aus dem HTML und gibt folgendes aus:

```
Seite 1 - https://example.com/page1
Seite 2 - https://example.com/page2
```

Dieses grundlegende Beispiel demonstriert, wie man SwiftSoup für das Parsen von HTML-Dokumenten nutzen kann. Indem man die Dokumentation von SwiftSoup weiter erforscht, kann man zahlreiche Methoden entdecken, um durch den HTML-Inhalt zu navigieren, zu suchen und ihn zu modifizieren, was Ihre Swift-Anwendungen befähigt, komplexe Webinhalte mit Leichtigkeit zu verarbeiten.
