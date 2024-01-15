---
title:                "HTML-Parsing"
html_title:           "Swift: HTML-Parsing"
simple_title:         "HTML-Parsing"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte man sich mit dem Parsen von HTML beschäftigen? Nun, es gibt viele Gründe, warum dies eine nützliche Fähigkeit für jeden Swift-Entwickler sein kann. Zum einen ermöglicht es das Extrahieren von Daten aus einer Webseite, die in HTML formatiert ist. Dies kann sehr hilfreich sein, wenn man zum Beispiel Daten von einer externen API erhält, die nur HTML zurückgibt. Außerdem kann das Parsen von HTML auch beim Scraping von Websites oder der Erstellung von nutzerdefinierten Web-Scrapern hilfreich sein.

## Wie geht das?
Um HTML zu parsen, gibt es verschiedene Möglichkeiten in Swift. Eine der beliebtesten ist die Verwendung von Bibliotheken wie SwiftSoup oder Kanna. Diese Bibliotheken ermöglichen es uns, HTML-Dateien zu laden und zu durchsuchen, um die gewünschten Informationen zu extrahieren.

### Ein Beispiel mit SwiftSoup
Um mit SwiftSoup zu arbeiten, muss man zunächst die Bibliothek mit dem Package Manager hinzufügen. Anschließend kann man den folgenden Code verwenden, um eine URL mit HTML zu laden und alle Links auf der Seite zu extrahieren:

```Swift
let url = URL(string: "https://www.example.com")!
let html = try! String(contentsOf: url)
let doc = try! SwiftSoup.parse(html)

for link in try! doc.select("a") {
    print(try! link.attr("href"))
}
```

Dies wird alle Links auf der Webseite ausgeben. Hier ist ein Beispiel der möglichen Ausgabe:

> https://www.example.com/page1
> https://www.example.com/page2
> https://www.example.com/page3

### Ein Beispiel mit Kanna
Kanna bietet eine ähnliche Funktionalität und kann auch mit dem Package Manager hinzugefügt werden. Hier ist ein Beispiel, wie man alle Bilder auf einer Webseite mit Kanna extrahieren kann:

```Swift
let url = URL(string: "https://www.example.com")!
let html = try! String(contentsOf: url)
let doc = try! HTML(html: html, encoding: .utf8)

for img in doc.xpath("//img") {
    print(img["src"])
}
```

Diese Beispiele sind nur ein kleiner Einblick in die Möglichkeiten des Parsens von HTML mit Swift. Die Bibliotheken bieten viele weitere Funktionen, um komplexe Aufgaben wie das Extrahieren von Text oder das Bearbeiten von HTML-Dokumenten zu ermöglichen.

## Tiefergehende Informationen
Beim Parsen von HTML ist es wichtig zu verstehen, wie die Struktur des HTML-Codes aussieht. Hierbei können sogenannte HTML-Parser helfen, die den HTML-Code analysieren und eine strukturierte Darstellung erstellen. Diese kann dann einfacher durchsucht werden, um die gewünschten Informationen zu extrahieren.

Ein weiterer wichtiger Aspekt beim Parsen von HTML ist das Verständnis von XPath-Ausdrücken. Diese Ausdrücke ermöglichen es, bestimmte Elemente in einem HTML-Dokument auszuwählen, was beim Durchsuchen und Extrahieren von Daten sehr hilft.

## Siehe auch
Für weitere Informationen und Tutorials zum Parsen von HTML in Swift empfehle ich diese Ressourcen:

- [SwiftSoup Dokumentation](https://github.com/scinfu/SwiftSoup#readme)
- [Kanna Dokumentation](https://github.com/tid-kijyun/Kanna#readme)
- [Swift XML Parser Tutorials](https://www.raywenderlich.com/7737-swift-xml-tutorial-how-to-parse-xml)
- [XPath Tutorial](https://www.w3schools.com/xml/xpath_intro.asp)