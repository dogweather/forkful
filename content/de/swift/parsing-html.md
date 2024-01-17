---
title:                "HTML analysieren"
html_title:           "Swift: HTML analysieren"
simple_title:         "HTML analysieren"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Parsen von HTML handelt es sich um den Prozess des Extrahierens von relevanten Informationen aus einer HTML-Seite. Programmierer tun dies, um automatisierte Aufgaben auszuführen, wie zum Beispiel das Sammeln von Daten oder das Erstellen von Webcrawler-Scripten.

## Wie geht's?
Das Parsen von HTML mit Swift ist einfach und kann mit der Verwendung von Drittanbieter-Bibliotheken wie SwiftSoup noch einfacher gemacht werden. Hier ist ein Beispiel dafür, wie man mit SwiftSoup eine HTML-Seite nach Titeln von Artikeln durchsucht:

```Swift
let html = "<html><head><title>Mein Artikel</title></head><body><h1>Erste Überschrift</h1><p>Erster Absatz</p><h2>Zweite Überschrift</h2><p>Zweiter Absatz</p></body></html>"

do {
    let doc: Document = try SwiftSoup.parse(html)
    let headers: Elements = try doc.getElementsByTag("h1")
    for header in headers {
        print(try header.text())
    }
} catch Exception.Error(let type, let message) {
    print("Fehler: \(message)")
} catch {
    print("Ein unbekannter Fehler ist aufgetreten")
}

// Output:
// Erste Überschrift
```
Das ```try``` Keyword und die ```do...catch``` Struktur werden verwendet, um Fehler abzufangen, die beim Parsen entstehen können. Die SwiftSoup-Bibliothek ermöglicht es uns, Elemente nach ihrem Tag oder Attributnamen zu durchsuchen und Text oder Attribute zu extrahieren.

## Tiefentauchen
Das Parsen von HTML hat eine lange Geschichte, die bis zu den Anfängen des World Wide Web im Jahr 1989 zurückreicht. Im Laufe der Jahre hat sich die Art und Weise, wie HTML interpretiert und geparst wird, geändert, was zur Entwicklung von verschiedenen Tools und Bibliotheken geführt hat. Alternativen zu SwiftSoup sind zum Beispiel HTMLKit oder Kanna, die beide ähnliche Funktionen bieten.

Die Implementierung von SwiftSoup basiert auf der Java-Bibliothek Jsoup und erfordert daher einige Kenntnisse über Java-Methoden und -Klassen. Dies kann für Swift-Entwickler ungewohnt sein, aber die Verwendung einer Third-Party-Bibliothek vereinfacht den Parsing-Prozess erheblich.

## Siehe auch
- [SwiftSoup Dokumentation](https://github.com/scinfu/SwiftSoup)
- [HTMLKit Bibliothek](https://github.com/iwasrobbed/HTMLKit)
- [Kanna Bibliothek](https://github.com/tid-kijyun/Kanna)