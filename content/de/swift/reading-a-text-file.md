---
title:                "Eine Textdatei lesen"
html_title:           "Swift: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
 Zum Lesen einer Textdatei gehört das Einlesen und Verarbeiten von Textinhalt aus einer Datei, die auf Ihrem Computer gespeichert ist. Programmierer tun dies, um Daten aus der Textdatei zu erhalten und sie in ihrer Anwendung zu verwenden.

## Wie geht das?
Hier ist ein Beispiel, wie Sie eine Textdatei in Swift einlesen können:
 
```Swift
let fileURL = URL(fileURLWithPath: "example.txt")
do {
    let text = try String(contentsOf: fileURL)
    print(text)
} catch {
    print("Fehler beim Einlesen der Datei.")
}
```

Die Ausgabe entspricht dem Inhalt der Textdatei "example.txt".

## Tiefer gehende Informationen
Das Lesen von Textdateien hat eine lange Geschichte in der Programmierung, da es eine grundlegende Methode ist, um Daten in eine Anwendung zu importieren. Alternativ können Programmierer auch auf Datenbanken zugreifen, um ihre Daten zu erhalten. Die Implementierung des Lesens einer Textdatei in einer Anwendung kann je nach Sprache und Framework unterschiedlich sein.

## Siehe auch
Wenn Sie mehr über das Lesen von Textdateien in Swift erfahren möchten, können Sie diese Ressourcen nutzen:
- [Offizielle Swift Dokumentation zu Dateimanager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Cheat Sheets - Datei I/O](https://medium.com/ios-os-x-development/swift-cheat-sheet-file-i-o-9df62593a1b1)