---
title:                "Überprüfung ob ein Verzeichnis existiert"
html_title:           "Swift: Überprüfung ob ein Verzeichnis existiert"
simple_title:         "Überprüfung ob ein Verzeichnis existiert"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Was & Warum?
Beim Programmieren kann es manchmal notwendig sein, zu überprüfen, ob ein bestimmtes Verzeichnis existiert. Dies kann hilfreich sein, um sicherzustellen, dass man auf die benötigten Dateien zugreifen kann, bevor man sie manipuliert oder liest. Es ist eine Vorsichtsmaßnahme, die dafür sorgt, dass das Programm reibungslos funktioniert und unerwartete Fehler vermieden werden.

# Wie geht's?
```Swift
let fileManager = FileManager.default
var isDirectory: ObjCBool = false
let path = "/Users/username/Documents"
if fileManager.fileExists(atPath: path, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("Das Verzeichnis existiert.")
    } else {
        print("Der Pfad ist nicht zu einem Verzeichnis.")
    }
} else {
    print("Das Verzeichnis existiert nicht.")
}
```

### Ausgabe:
```Das Verzeichnis existiert.```

# Tiefgehende Einblicke
- Historischer Kontext: Die Funktionalität, um zu überprüfen, ob ein Verzeichnis existiert, wurde in das iOS-Betriebssystem aufgenommen, um frühere Methoden wie "isReadableFile" und "isWritableFile" zu ersetzen. Es ist Teil des Foundation Frameworks, das seit den Anfängen von Swift im Jahr 2014 verfügbar ist.
- Alternativen: Statt "fileExists" kann auch "fileExistsAtPath" verwendet werden, um direkt den Pfad zu überprüfen, anstatt eine Boolean-Variable zu verwenden.
- Implementierungsdetails: Die Funktion "fileExists" verwendet den FileManager und die Methode "fileExistsAtPath" hinter den Kulissen, um zu überprüfen, ob das Verzeichnis existiert. Um die Variable "isDirectory" zu aktualisieren, muss sie als "inout" gekennzeichnet werden, damit sie innerhalb der Funktion geändert werden kann.

# Sieh auch
- [Apple Dokumentation zu "fileExists"](https://developer.apple.com/documentation/foundation/filemanager/1408678-fileexists)
- [Stack Overflow Diskussion über "fileExists"](https://stackoverflow.com/questions/27834155/check-if-file-exists-and-is-a-directory-or-symlink-in-swift)