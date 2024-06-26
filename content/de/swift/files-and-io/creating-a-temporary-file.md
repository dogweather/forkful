---
date: 2024-01-20 17:41:11.075675-07:00
description: "Wie geht das?: Tempor\xE4re Dateien haben seit den fr\xFChesten Tagen\
  \ der Programmierung ihren Platz. Sie erzeugen eine geringere Last auf dem\u2026"
lastmod: '2024-04-05T22:51:08.782764-06:00'
model: gpt-4-1106-preview
summary: "Tempor\xE4re Dateien haben seit den fr\xFChesten Tagen der Programmierung\
  \ ihren Platz."
title: "Erstellung einer tempor\xE4ren Datei"
weight: 21
---

## Wie geht das?:
```Swift
import Foundation

// Temporäre Datei erstellen
let tempDirectoryURL = FileManager.default.temporaryDirectory
let tempFileURL = tempDirectoryURL.appendingPathComponent("temp.txt")

// In die Datei schreiben
let content = "Das ist temporär!"
do {
    try content.write(to: tempFileURL, atomically: true, encoding: .utf8)
    print("Temporäre Datei erstellt unter: \(tempFileURL.path)")
} catch {
    print("Fehler beim Schreiben der temporären Datei: \(error)")
}

// Temporäre Datei lesen
do {
    let readContent = try String(contentsOf: tempFileURL, encoding: .utf8)
    print("Inhalt der temporären Datei: \(readContent)")
} catch {
    print("Fehler beim Lesen der temporären Datei: \(error)")
}

// Temporäre Datei löschen
do {
    try FileManager.default.removeItem(at: tempFileURL)
    print("Temporäre Datei gelöscht.")
} catch {
    print("Fehler beim Löschen der temporären Datei: \(error)")
}
```

Output:
```
Temporäre Datei erstellt unter: /tmp/temp.txt
Inhalt der temporären Datei: Das ist temporär!
Temporäre Datei gelöscht.
```

## Deep Dive
Temporäre Dateien haben seit den frühesten Tagen der Programmierung ihren Platz. Sie erzeugen eine geringere Last auf dem Festplattenspeicher und können Datenflüsse in Prozessen erleichtern, die keinen dauerhaften Speicher benötigen.

Alternativ zur manuellen Erstellung kann `URLSession` verwendet werden, um Daten vorübergehend während Netzwerkanfragen zu speichern. Implementierungsdetails umfassen meist automatische Löschvorgänge nach Gebrauch oder beim Beenden der Anwendung.

Für komplexere Bedingungen gibt es Bibliotheken wie Swift’s `Temporary` Modul, welches zusätzliche Features für Dateierstellung und -management bietet. So stellen Threadsicherheit und Ausnahmebehandlung kein größeres Problem dar.

## See Also
- [FileManager Class](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift `Temporary` Module Documentation](https://github.com/JohnSundell/Files)
