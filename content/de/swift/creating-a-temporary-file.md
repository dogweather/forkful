---
title:    "Swift: Erstellen einer temporären Datei"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Erstellen von temporären Dateien ist eine nützliche Funktion in der Swift Programmierung, da es Ihnen ermöglicht, vorübergehende Daten zu speichern, die nicht dauerhaft auf Ihrem Gerät gespeichert werden müssen. Dies kann hilfreich sein, wenn Sie beispielsweise temporäre Dateien für Caches oder Downloads benötigen.

## Wie man
Die Erstellung einer temporären Datei ist in Swift relativ einfach. Hier ist ein Beispiel, wie Sie eine temporäre Datei mit dem Namen "sample.txt" erstellen können:

```Swift
let tempDir = FileManager.default.temporaryDirectory
let tempFileURL = tempDir.appendingPathComponent("sample.txt")

// Hier können Sie Daten in die temporäre Datei schreiben oder sie mit Inhalt füllen

try? "Dies ist ein Beispieltext".write(to: tempFileURL, atomically: true, encoding: String.Encoding.utf8)
```

Um auf die temporäre Datei zuzugreifen und sie zu verwenden, können Sie einfach mit ihrem URL umgehen. Im obigen Beispiel haben wir sie "tempFileURL" genannt. Hier ist ein einfaches Beispiel, wie Sie den Inhalt der temporären Datei ausgeben können:

```Swift
let fileContents = try? String(contentsOf: tempFileURL, encoding: String.Encoding.utf8)
print(fileContents)
```

Wenn Sie mit der temporären Datei fertig sind, können Sie sie einfach löschen, indem Sie den Befehl "remove(at:)" verwenden:

```Swift
try? FileManager.default.removeItem(at: tempFileURL)
```

## Tiefentauchen
Die Verwendung von temporären Dateien hat viele praktische Anwendungsfälle in der Swift Programmierung. Sie ermöglichen es Ihnen, vorübergehende Daten schnell und effizient zu speichern, ohne dass sie dauerhaft auf Ihrem Gerät gespeichert werden müssen. Dies ist besonders nützlich, wenn Sie mit sensiblen Daten umgehen und keine Spuren davon auf Ihrem Gerät hinterlassen möchten.

Wenn Sie mehr über das Erstellen von temporären Dateien in Swift erfahren möchten, können Sie sich die offizielle Dokumentation von Apple ansehen. Dort finden Sie detaillierte Informationen und weitere Beispiele.

## Siehe auch
- [Offizielle Dokumentation von Apple über das Erstellen von temporären Dateien](https://developer.apple.com/documentation/foundation/filemanager/1407723-temporarydirectory)
- [Ein Tutorial über die Verwendung von temporären Dateien in Swift](https://www.iosapptemplates.com/blog/swift/temporary-files-swift) 
- [StackOverflow Thread über das Erstellen und Verwenden von temporären Dateien in Swift](https://stackoverflow.com/questions/24581517/create-a-temporary-file-in-swift)