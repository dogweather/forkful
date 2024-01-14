---
title:                "Swift: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Aufgabe in der Swift-Programmierung. Es hilft Entwicklern, sicherzustellen, dass sie auf die richtigen Dateien zugreifen und ihre Anwendungen reibungslos laufen können.

## Wie man es macht

Das Überprüfen, ob ein Verzeichnis existiert, kann mit der ```SwiftFileManager.default.fileExists(atPath: String) -> Bool``` Methode durchgeführt werden. Diese Methode gibt einen Booleschen Wert zurück, der angibt, ob das Verzeichnis an dem angegebenen Pfad existiert. Hier ist ein Beispielcode, der den Nutzer auffordert, einen Pfad einzugeben und dann überprüft, ob ein Verzeichnis an diesem Pfad existiert:

```Swift
import Foundation

let fileManager = FileManager.default
print("Geben Sie einen Pfad ein:")
let path = readLine()

if let path = path {
    if fileManager.fileExists(atPath: path) {
        print("Das Verzeichnis existiert.")
    } else {
        print("Das Verzeichnis existiert nicht.")
    }
}
```

Beispiel-Eingabe: /Users/Username/Documents
Beispiel-Ausgabe: Das Verzeichnis existiert.

## Vertiefung

Es gibt verschiedene Methoden, mit denen Entwickler überprüfen können, ob ein Verzeichnis existiert, darunter auch die Verwendung von ```SwiftNSURL``` und ```SwiftURL```. Diese Methoden ermöglichen es auch, die verschiedenen Attribute eines Verzeichnisses, wie z.B. die Größe und die erstellungs- und änderungsdatum, abzurufen. Es ist wichtig zu beachten, dass die meisten dieser Methoden in Swift 4.2 veraltet sind und Entwickler stattdessen die neueren APIs verwenden sollten.

## Siehe auch

- [Dokumentation zu FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Überprüfen, ob eine Datei existiert in Swift](https://stackoverflow.com/questions/26555050/how-to-check-if-a-file-exists-in-swift) 
- [Informationen zu Swift 4.2 APIs](https://swift.org/blog/5-0-release-process/)