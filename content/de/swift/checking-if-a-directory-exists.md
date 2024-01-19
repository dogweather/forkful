---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Lua: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist ein grundlegendes Verfahren, mit dem wir feststellen, ob ein bestimmter Speicherort in unserer Dateistruktur vorhanden ist. Programmierer machen dies um sicherzustellen, dass sie keine Daten lesen oder schreiben, wo es nicht zulässig oder möglich ist.

## Wie macht man das:

In Swift können Sie mit dem FileManager checken, ob ein Verzeichnis existiert. Hier ist ein einfaches Beispiel:

```Swift
import Foundation

let fileManager = FileManager.default
let directoryPath = "/Users/username/Documents/Test"

var isDirectory: ObjCBool = false
if fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        // Verzeichnis existiert
        print("\(directoryPath) existiert.")
    } else {
        // Datei existiert, aber kein Verzeichnis
        print("\(directoryPath) ist eine Datei.")
    }
} else {
    // Verzeichnis existiert nicht
    print("\(directoryPath) existiert nicht.")
}
```

## Vertiefung:

Historisch gesehen ermöglichen fast alle Betriebssysteme und Programmiersprachen die Überprüfung, ob ein Verzeichnis existiert, jedoch auf unterschiedliche Weisen. Bei Python beispielsweise, benutzt man `os.path.isdir()` und bei Java setzt man `Files.isDirectory()` ein.

Eine Alternative zur Verwendung des `fileExists(atPath:isDirectory:)` von Swift besteht darin, das `FileAttributeType`-Attribut des Verzeichnisses zu überprüfen. Wenn dieses Attribut auf `NSFileTypeDirectory` eingestellt ist, bedeutet das, es handelt sich um ein Verzeichnis.

Trotzdem hat `fileExists(atPath:isDirectory:)` Vorteile: es ist direkt und simpel und das 'Bool' Argument akzeptiert eine Variable, die dann die Information beinhaltet, ob es ein Directory ist.

## Weitere Informationen:

- Swift Documentation: [FileManager.fileExists(atPath:isDirectory:)](https://developer.apple.com/documentation/foundation/filemanager/1410277-fileexists)
- StackOverflow: [How to check if a file or directory exists](https://stackoverflow.com/questions/24097826/read-and-write-data-from-text-file)