---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was und warum?

Die Erstellung temporärer Dateien ermöglicht es Programmierern, Daten vorübergehend zu speichern und zu bearbeiten. Das ist äußerst nützlich für Aufgaben wie die Verarbeitung großer Datenmengen, bei denen es ineffizient wäre, alle Daten im Arbeitsspeicher zu halten.

## So geht's:

Erstellen wir eine temporäre Datei in Swift. Hier verwenden wir das `FileManager` Toolkit zur Dateiverarbeitung:

```Swift
import Foundation

let fileName = ProcessInfo.processInfo.globallyUniqueString
let temporaryDirectoryURL = URL(fileURLWithPath: NSTemporaryDirectory(), isDirectory: true)
let temporaryFileURL = temporaryDirectoryURL.appendingPathComponent(fileName)

do {
    try "Hallo Welt!".write(to: temporaryFileURL, atomically: true, encoding: String.Encoding.utf8)
    print("Temporäre Datei erfolgreich erstellt: \(temporaryFileURL.path)")
} catch {
    print("Fehler beim Erstellen der temporären Datei: \(error)")
}
```

Wenn du dieses Skript ausführst, wird eine temporäre Datei mit einem einzigartigen Namen in deinem temporären Verzeichnis erstellt und der Text "Hallo Welt!" wird in diese Datei geschrieben.

## Vertiefung

Historisch gesehen werden temporäre Dateien seit Anbeginn der Computerprogrammierung verwendet, um Arbeitsspeicher zu sparen. Sie sind ein wertvolles Werkzeug, wenn du große Mengen an Daten effizient verarbeiten musst.

Allerdings gibt es Alternativen zur Verwendung temporärer Dateien, wie z.B. die Datenpufferung in Datenbanken oder die Verwendung von Cloud-Datenspeicherdiensten. Welcher Ansatz für dich am besten passt, hängt von deinen spezifischen Anforderungen ab.

Was die Implementierung betrifft, so verwendet `NSTemporaryDirectory()` in unserem Code oben die TEMP-Variablen deiner Umgebung, um ein temporäres Verzeichnis zu erstellen. Das ist sowohl auf Linux- als auch auf macOS-Systemen wirksam.

## Siehe auch

Hier sind einige nützliche Links, die dir weiterhelfen könnten:

1. Apple Dokumentation : [FileManager](https://developer.apple.com/documentation/foundation/filemanager)
2. StackOverflow: [Temporary Files in Swift](https://stackoverflow.com/questions/27327067/temporary-files-in-swift)