---
title:                "Eine Textdatei schreiben"
date:                  2024-01-19
html_title:           "Arduino: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben in eine Textdatei bedeutet, dass Daten dauerhaft gespeichert werden. Programmierer nutzen dies, um Daten zu loggen, Einstellungen zu speichern oder Informationen zwischen Sessions zu übertragen.

## How to:

Swift bietet `String` die Methode `write(to:atomically:encoding:)`, um Daten einfach in Dateien zu schreiben.

```Swift
import Foundation

let text = "Hallo, Welt!"
if let dir = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
    let fileURL = dir.appendingPathComponent("meineDatei.txt")
    
    do {
        try text.write(to: fileURL, atomically: false, encoding: .utf8)
        print("Datei erfolgreich gespeichert.")
    } catch {
        print("Fehler beim Schreiben der Datei: \(error)")
    }
}
```

Beim Ausführen schreibt das Programm "Hallo, Welt!" in die Datei `meineDatei.txt` im Dokumentenverzeichnis des Nutzers.

## Deep Dive

Historisch gesehen wurde in Swift `NSFileManager` genutzt, das Teil von Foundation ist. Mittlerweile bietet `FileManager` modernere und sicherere Schnittstellen. Als Alternative kannst du Streams oder niedrigere APIs wie `fwrite` in POSIX verwenden, falls nötig. Beim Schreiben von Textdateien sollte man auf korrekte Kodierung achten (meistens UTF-8), um Zeichensatzprobleme zu vermeiden.

## See Also

- Swift Documentation zum `FileManager`: [FileManager | Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- Swift Standard Library zum String Handling: [String | Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
- Apple Guide zu Dateisystem-Interaktionen: [File System Programming Guide](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)
