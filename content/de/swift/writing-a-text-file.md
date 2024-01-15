---
title:                "Das Schreiben einer Textdatei"
html_title:           "Swift: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Schreiben von Textdateien ist eine wichtige Fähigkeit für jeden Swift-Programmierer. Es ermöglicht uns, Daten auf einfache Weise zu speichern und zu verarbeiten, ohne auf spezielle Datentypen oder Komplexität zurückgreifen zu müssen.

# Wie geht das?

Um eine Textdatei in Swift zu schreiben, können wir die `String` Klasse verwenden. Mit der `write(to:atomically:encoding:)` Methode können wir einen String direkt in eine Datei schreiben. Hier ist ein Beispielcode:

```Swift
let text = "Hallo Welt!"
let fileURL = URL(fileURLWithPath: "output.txt")
try text.write(to: fileURL, atomically: true, encoding: .utf8)
```

Mit diesem Code wird der String "Hallo Welt!" in die Datei "output.txt" geschrieben. Der Parameter `atomically` stellt sicher, dass die Datei sicher geschrieben wird und `encoding` gibt an, dass der Text in UTF-8 codiert sein soll.

# Tief einsteigen

Es gibt auch andere Möglichkeiten, eine Textdatei in Swift zu schreiben. Zum Beispiel können wir den `FileManager` verwenden, um eine neue Datei anzulegen und den `OutputStream` verwenden, um den Text in die Datei zu schreiben. Dies bietet mehr Kontrolle über die Schreiboperation und die Dateinamen. Hier ist ein Beispielcode:

```Swift
let text = "Hallo Welt!"
let fileURL = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent("output.txt")
FileManager.default.createFile(atPath: fileURL.path, contents: nil, attributes: nil)
if let outputStream = OutputStream(url: fileURL, append: true) {
    outputStream.open()
    let bytesWritten = outputStream.write(text, maxLength: text.lengthOfBytes(using: .utf8))
    print("Bytes geschrieben: \(bytesWritten)")
    outputStream.close()
}
```

Mit diesem Code wird die Textdatei erstellt und der Text wird mithilfe des `OutputStreams` in die Datei geschrieben. Wir können auch die Anzahl der Bytes überwachen, die geschrieben wurden, indem wir die Rückgabe der `write` Methode überprüfen.

# Siehe auch

- [Swift Dokumentation zu Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Tutorial zu Textverarbeitung in Swift](https://www.raywenderlich.com/147086/text-processing-swift-3)
- [Kostenloser Swift-Kurs von Apple](https://developer.apple.com/videos/play/wwdc2018/404/)