---
title:                "Swift: Eine Textdatei lesen"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Textdateien sind ein wichtiger Bestandteil der Programmierung in Swift. Sie ermöglichen es uns, Daten auszulesen und zu verarbeiten, die in einem menschenlesbaren Format gespeichert sind. In diesem Blogbeitrag werden wir uns genauer mit dem Lesen von Textdateien in Swift befassen und zeigen, wie einfach es ist, dies in unseren eigenen Projekten umzusetzen.

## Wie geht man vor

Um eine Textdatei in Swift zu lesen, müssen wir zunächst eine Instanz der Klasse `FileManager` erstellen. Diese ermöglicht es uns, auf Dateien in unserem Dateisystem zuzugreifen. Anschließend können wir die Methode `contents(atPath:)` verwenden, um den Inhalt der Textdatei als `Data`-Objekt zu erhalten.

```Swift
if let fileManager = FileManager.default,
   let fileData = fileManager.contents(atPath: "textdatei.txt") {
    // den Inhalt der Textdatei verarbeiten
    let text = String(data: fileData, encoding: .utf8)
    print(text)
} else {
    // Fehlerbehandlung, falls die Datei nicht gefunden werden konnte
    print("Die Datei konnte nicht gefunden werden.")
}
```
Das `Data`-Objekt, das wir erhalten, können wir anschließend in einen `String` umwandeln und somit den Inhalt der Textdatei auslesen. Hier verwenden wir die Encodierung `.utf8`, da diese in den meisten Fällen die gängigste für Textdateien ist. Falls die Datei nicht gefunden werden konnte, geben wir eine Fehlermeldung aus.

## Tiefergehend

Beim Lesen von Textdateien in Swift müssen wir uns auch mit der Verarbeitung von Zeilenumbrüchen auseinandersetzen. Diese werden in einem `String`-Objekt durch das Zeichen `\n` dargestellt. Wenn wir also jede Zeile der Textdatei einzeln verarbeiten möchten, können wir die Methode `components(separatedBy:)` verwenden und `\n` als Trennzeichen angeben. Dies gibt uns ein Array von `String`-Objekten, wobei jedes Element eine Zeile aus der Textdatei darstellt.

```Swift
if let fileManager = FileManager.default,
   let fileData = fileManager.contents(atPath: "textdatei.txt") {
    let text = String(data: fileData, encoding: .utf8)
    // das Array words enthält jetzt jede Zeile der Textdatei als String
    let words = text?.components(separatedBy: "\n")
    
    // jedes Element des Arrays kann jetzt einzeln verarbeitet werden
    for word in words {
    print(word)
    }
}
```

## Siehe auch

- [Apple Dokumentation zu `FileManager`](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift Standard Library Referenz zu `String`](https://developer.apple.com/documentation/swift/string)