---
title:                "Swift: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist ein wichtiger Teil der Programmierung in Swift. Es kann hilfreich sein, um Zwischenergebnisse zu speichern oder um temporäre Daten während der Ausführung des Programms zu verwalten.

## Wie es geht

Um eine temporäre Datei in Swift zu erstellen, folgen Sie diesen Schritten:

1. Importieren Sie die Foundation-Bibliothek mit `import Foundation`
2. Verwenden Sie die `NSTemporaryDirectory()`-Methode, um den Pfad des temporären Verzeichnisses zu erhalten. Dieser Pfad ändert sich jedes Mal, wenn das Programm ausgeführt wird.
3. Verwenden Sie die `URL(fileURLWithPath:isDirectory:)`-Methode, um eine URL für die temporäre Datei zu erstellen. Geben Sie den Pfad des temporären Verzeichnisses als Argument für den Parameter `fileURLWithPath` an und setzen Sie `isDirectory` auf `false`.
4. Verwenden Sie die `write(to:atomically:encoding:)`-Methode, um Daten in die temporäre Datei zu schreiben. Sie können entweder eine Zeichenfolge oder eine Datenstruktur als Argument für den Parameter `write(to:atomically:encoding:)` angeben.
5. Sie können auch die `URL.fileURLWithoutPathExtension()`-Methode verwenden, um das Verzeichnis der temporären Datei zu erhalten.

```Swift
import Foundation
let tempDir = NSTemporaryDirectory()
let tempURL = URL(fileURLWithPath: tempDir, isDirectory: false)
let text = "Dies ist ein Beispieltext"
try? text.write(to: tempURL, atomically: true, encoding: .utf8)
let dirURL = tempURL.deletingPathExtension()
```

Die obigen Schritte erstellen eine temporäre Datei im Systemtemporärbereich und schreiben den Beispieltext hinein. Wir können auch sehen, dass wir das Verzeichnis der temporären Datei bekommen können, indem wir die Dateinamenerweiterung löschen.

## Tiefergehende Einblicke

Das Erstellen von temporären Dateien kann auch nützlich sein, um sicherzustellen, dass bestimmte Daten während der Laufzeit eines Programms nicht dauerhaft gespeichert bleiben. Zum Beispiel können Sie temporäre Dateien verwenden, um Zwischenergebnisse zu speichern, die nicht relevant sind, sobald Ihr Programm seine Aufgabe erledigt hat.

Es ist auch wichtig zu beachten, dass temporäre Dateien nicht immer automatisch gelöscht werden. Sie müssen sicherstellen, dass Sie die temporäre Datei löschen, sobald Sie sie nicht mehr benötigen, um Speicherplatz zu sparen.

## Siehe auch

- [Foundation Framework Reference](https://developer.apple.com/documentation/foundation)
- [Temporary Files in Swift](https://swifter.tips/temporary-file/)