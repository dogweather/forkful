---
title:                "Swift: Erstellen einer temporären Datei"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Warum man eine temporäre Datei erstellen könnte

Die Erstellung einer temporären Datei ist eine nützliche Funktion in der Swift-Programmierung. Sie ermöglicht es, Daten vorübergehend zu speichern, ohne dass sie dauerhaft auf dem Gerät gespeichert werden müssen. Das kann hilfreich sein, wenn man beispielsweise eine Datei aus dem Internet herunterladen und verarbeiten möchte, ohne sie auf dem Gerät dauerhaft zu behalten.

Wie erstelle ich eine temporäre Datei in Swift?

Um eine temporäre Datei in Swift zu erstellen, können wir die Funktion `NSTemporaryDirectory()` verwenden. Diese gibt uns den Pfad des temporären Verzeichnisses auf dem Gerät zurück. Wir können diesen Pfad dann zusammen mit einer zufällig generierten Dateinamen verwenden, um unsere temporäre Datei zu erstellen. Hier ein Beispielcode:

```Swift
let temporaryPath = NSTemporaryDirectory() // Pfad des temporären Verzeichnisses
let tempFileName = String.random(length: 10) // Zufällig generierter Dateiname
let tempFilePath = temporaryPath.appending(tempFileName) // Vollständiger Pfad der temporären Datei
FileManager.default.createFile(atPath: tempFilePath, contents: nil, attributes: nil) // Erstellt die temporäre Datei
```

In diesem Beispiel verwenden wir eine zufällig generierte Dateinamenfunktion `String.random`. Dies stellt sicher, dass unsere temporäre Datei immer einen eindeutigen Namen hat, um eventuelle Konflikte zu vermeiden. Die Funktion `FileManager.default.createFile` erstellt dann die eigentliche temporäre Datei.

Das Ergebnis des oben genannten Codes wird eine neue temporäre Datei an einem zufälligen Ort auf dem Gerät sein. Dieser Pfad kann dann verwendet werden, um Daten in die temporäre Datei zu schreiben oder sie zu lesen.

Tiefer Einblick in die Erstellung von temporären Dateien

Die Erstellung einer temporären Datei ist ein nützliches Konzept, aber es gibt einige Dinge zu beachten. Zum Beispiel sollten temporäre Dateien immer gelöscht werden, sobald sie nicht mehr benötigt werden. Dies kann durch die Verwendung der Funktion `FileManager.default.removeItem` erreicht werden.

Es ist auch wichtig zu beachten, dass die Funktion `NSTemporaryDirectory()` manchmal einen Pfad zurückgeben kann, der nicht existiert. In diesem Fall sollte man immer zuerst prüfen, ob der Pfad tatsächlich existiert, bevor man versucht, eine temporäre Datei zu erstellen.

Zusätzliche Informationen zur Verwendung von temporären Dateien und die Vermeidung von potenziellen Problemen finden Sie in der offiziellen Dokumentation von Apple zu diesem Thema.

Siehe auch

- [NSTemporaryDirectory() Dokumentation](https://developer.apple.com/documentation/foundation/1409213-nstemporarydirectory)
- [FileManager Dokumentation](https://developer.apple.com/documentation/foundation/filemanager)
- [Offizielles Swift Tutorial](https://docs.swift.org/swift-book/LanguageGuide/Extensions.html)