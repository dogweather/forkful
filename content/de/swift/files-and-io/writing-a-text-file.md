---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:47.767691-07:00
description: "Das Schreiben einer Textdatei in Swift erm\xF6glicht es Ihnen, Zeichenketten-Daten\
  \ dauerhaft auf dem Dateisystem zu speichern, was f\xFCr Aufgaben wie das\u2026"
lastmod: '2024-03-13T22:44:54.243202-06:00'
model: gpt-4-0125-preview
summary: "Das Schreiben einer Textdatei in Swift erm\xF6glicht es Ihnen, Zeichenketten-Daten\
  \ dauerhaft auf dem Dateisystem zu speichern, was f\xFCr Aufgaben wie das\u2026"
title: Eine Textdatei schreiben
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben einer Textdatei in Swift ermöglicht es Ihnen, Zeichenketten-Daten dauerhaft auf dem Dateisystem zu speichern, was für Aufgaben wie das Speichern von Konfigurationseinstellungen, Benutzerdaten oder Protokollen wesentlich ist. Programmierer tun dies oft, um Daten zwischen App-Starts zu erhalten, Daten zwischen verschiedenen Teilen einer Anwendung zu teilen oder Daten zu exportieren, die von anderen Programmen verwendet werden sollen.

## Wie zu:

### Verwendung der Swift Standardbibliothek

Die Standardbibliothek von Swift umfasst alle notwendigen Werkzeuge zum Schreiben von Textdateien. Hier ist ein grundlegender Ansatz:

```swift
import Foundation

let content = "Hallo, Wired-Leser! Swift zu lernen macht Spaß."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/beispiel.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("Datei erfolgreich geschrieben")
} catch let error as NSError {
    print("Schreiben zur URL fehlgeschlagen: \(fileName), Fehler: " + error.localizedDescription)
}
```

Dieser Codeausschnitt schreibt eine Zeichenkette in eine Datei namens `beispiel.txt` im Dokumentenverzeichnis. Er behandelt mögliche Fehler mit Swifts Fehlerbehandlung do-try-catch.

### Verwendung von FileManager für mehr Kontrolle

Für mehr Kontrolle über Dateiattribute oder um zu überprüfen, ob die Datei bereits existiert, kann `FileManager` verwendet werden:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("beispiel.txt")
    let content = "Swift für Dateiverwaltung zu erkunden ist aufschlussreich."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("Datei existiert bereits")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("Datei erfolgreich erstellt und geschrieben")
        } catch {
            print("Fehler beim Schreiben der Datei: \(error)")
        }
    }
}
```

### Verwendung von Drittanbieter-Bibliotheken

Eine beliebte Drittanbieter-Bibliothek für Dateisystemoperationen in Swift ist `Files` von John Sundell:

Fügen Sie zunächst Files Ihrem Projekt hinzu, üblicherweise über den Swift Package Manager.

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "IhrPaketName",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "IhrZielName",
            dependencies: ["Files"]),
    ]
)
```

Verwenden Sie es dann, um in eine Datei zu schreiben:

```swift
import Files

do {
    let file = try File(path: "/pfad/zu/ihrem/verzeichnis/beispiel.txt")
    try file.write(string: "Swift und die Files-Bibliothek bilden eine starke Kombination.")
    print("Datei erfolgreich mit Files-Bibliothek geschrieben.")
} catch {
    print("Ein Fehler ist aufgetreten: \(error)")
}
```

Mit der `Files`-Bibliothek wird die Dateibehandlung unkomplizierter, sodass Sie sich auf die Geschäftslogik Ihrer Anwendung konzentrieren können anstatt auf die Details der Dateiverwaltung.
