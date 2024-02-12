---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-02-03T19:08:47.374857-07:00
model:                 gpt-4-0125-preview
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Es ist unerlässlich zu überprüfen, ob ein Verzeichnis im Dateisystem existiert, um Dateistrukturen innerhalb Ihrer Swift-Anwendungen zu verwalten. Diese Aufgabe ermöglicht es Entwicklern, die Präsenz von Verzeichnissen zu überprüfen, bevor sie versuchen, aus diesen zu lesen oder in diese zu schreiben, und somit mögliche Laufzeitfehler zu vermeiden.

## Wie geht das:

Das Foundation-Framework von Swift stellt die `FileManager`-Klasse bereit, die Methoden zur Verwaltung des Dateisystems bietet. Sie können `FileManager` verwenden, um zu überprüfen, ob ein Verzeichnis existiert. Hier ist ein Code-Snippet, wie man das macht:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Verzeichnis existiert")
} else {
    print("Verzeichnis existiert nicht")
}
```

Allerdings überprüft dies sowohl Dateien als auch Verzeichnisse. Wenn Sie speziell überprüfen möchten, ob ein Verzeichnis existiert, müssen Sie einen Zeiger auf einen Boolean-Wert in `isDirectory` übergeben:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Verzeichnis existiert")
} else {
    print("Verzeichnis existiert nicht")
}
```

### Verwendung einer Drittanbieter-Bibliothek

Bis jetzt erfordert die Überprüfung der Existenz eines Verzeichnisses in Swift normalerweise keine Drittanbieter-Bibliotheken aufgrund der Robustheit der `FileManager`-Klasse. Für komplexere Dateimanipulationen und Überprüfungen bieten Bibliotheken wie **Files** von John Sundell jedoch eine Swift-freundlichere API.

So könnten Sie es verwenden:

Zuerst fügen Sie Files Ihrem Projekt über den Swift Package Manager hinzu.

Dann können Sie so überprüfen, ob ein Verzeichnis existiert:

```swift
import Files

do {
    _ = try Folder(path: "/path/to/your/directory")
    print("Verzeichnis existiert")
} catch {
    print("Verzeichnis existiert nicht")
}
```

Hinweis: Da sich Drittanbieter-Bibliotheken ändern können, beziehen Sie sich immer auf die neueste Dokumentation für die Verwendung und die besten Praktiken.
