---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:58:39.903064-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
In Swift zu überprüfen, ob ein Verzeichnis existiert, bedeutet, sicherzustellen, dass ein bestimmter Pfad auf eine echte Position im Dateisystem verweist. Programmierer machen das, um Fehler zu vermeiden, die auftreten, wenn sie versuchen, auf ein nicht existierendes Verzeichnis zuzugreifen oder darin zu arbeiten.

## Wie geht das?
Swift bietet das `FileManager`-Objekt, um mit dem Dateisystem zu arbeiten. Um zu überprüfen, ob ein Verzeichnis existiert, nutzt du die `fileExists(atPath:)`-Methode. Hier ist ein einfaches Beispiel:

```Swift
import Foundation

let fileManager = FileManager.default
let path = "/Pfad/zum/Verzeichnis"

if fileManager.fileExists(atPath: path) {
    print("Das Verzeichnis existiert.")
} else {
    print("Das Verzeichnis existiert nicht.")
}
```

Ausgabe könnte sein:
```
Das Verzeichnis existiert.
```
oder
```
Das Verzeichnis existiert nicht.
```

## Tiefgang
Historisch gesehen haben Programmiersprachen oft Funktionen zur Dateisystem-Manipulation bereitgestellt. In Swift erledigt das `FileManager`‑Objekt diese Aufgabe. Es gibt Alternativen, wie die Verwendung von `attributesOfItem(atPath:)`, was aber mehr Informationen zurückgibt und für einfaches Existenz-Checken überdimensioniert ist.

Beim Implementieren ist zu beachten, dass die `fileExists(atPath:)`-Methode nicht zwischen Dateien und Verzeichnissen unterscheidet. Wenn du ausschließlich nach einem Verzeichnis suchen möchtest, kannst du den `isDirectory`-Parameter so verwenden:

```Swift
var isDir: ObjCBool = false
if fileManager.fileExists(atPath: path, isDirectory: &isDir), isDir.boolValue {
    print("Es ist ein Verzeichnis.")
} else {
    print("Es ist keine Verzeichnis oder es existiert nicht.")
}
```

## Siehe auch
- [FileManager Class Reference](https://developer.apple.com/documentation/foundation/filemanager)
- [Working with Directories in Swift on iOS](https://www.raywenderlich.com/660-using-the-filemanager-to-work-with-files-in-ios)
- [Swift Documentation](https://swift.org/documentation/)
