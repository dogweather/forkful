---
title:    "Swift: Erstellen einer temporären Datei"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Warum 

Das Erstellen temporärer Dateien ist eine nützliche Technik, wenn Sie Daten speichern oder verarbeiten müssen, die nur vorübergehend benötigt werden. Diese Dateien können zum Beispiel in einer App verwendet werden, um temporäre Zwischenergebnisse zu speichern oder um schnell große Datenmengen zu speichern und zu verarbeiten.

## Wie erstellt man eine temporäre Datei

Das Erstellen einer temporären Datei ist in Swift sehr einfach. Zunächst müssen Sie ein Objekt vom Typ `URL` erstellen, welches den Pfad und den Dateinamen für Ihre temporäre Datei enthält. Dies kann zum Beispiel in einer Funktion erfolgen, die den Dateinamen und die Dateiendung als Parameter entgegennimmt.

```Swift
func createTemporaryFile(fileName: String, fileExtension: String) -> URL {
    let temporaryDirectory = URL(fileURLWithPath: NSTemporaryDirectory())
    let fileURL = temporaryDirectory.appendingPathComponent(fileName + "." + fileExtension)
    
    return fileURL
}
```

Als nächstes muss die tatsächliche Datei erstellt werden, indem die Methode `createFile(atPath:contents:attributes:)` auf der Klasse `FileManager` aufgerufen wird. Diese Methode erstellt eine neue leere Datei unter dem angegebenen Pfad.

```Swift
let fileURL = createTemporaryFile(fileName: "temporary", fileExtension: "txt")
let fileManager = FileManager.default

do {
    try fileManager.createFile(atPath: fileURL.path, contents: nil, attributes: nil)
} catch {
    print("Error creating temporary file: \(error)")
}
```

Nachdem die Datei erstellt wurde, können Sie wie gewohnt auf sie zugreifen und Daten speichern oder verarbeiten. Beachten Sie jedoch, dass die Datei nur temporär gespeichert wird und beim Beenden Ihrer App wieder gelöscht wird.

## Tiefergehende Informationen

Sollten Sie mehr Kontrolle über die Erstellung und Verwaltung Ihrer temporären Dateien haben wollen, können Sie dies über die `TemporaryFile`-Klasse erreichen. Diese Klasse bietet zusätzliche Funktionen wie das Setzen von Ablaufdaten oder das Löschen der Datei bei Bedarf. Bevor Sie jedoch eine solche Klasse implementieren, sollten Sie sicherstellen, dass Ihre Anforderungen nicht bereits durch die Standardfunktionen von Swift abgedeckt werden.

## Siehe auch

- [Apple Developer Documentation - Creating and Handling Temporary Files](https://developer.apple.com/documentation/foundation/filesystem/coping_with_temporary_files)
- [Medium Article - Managing Temporary Files in Swift](https://medium.com/@kenju_lee/managing-temporary-files-in-swift-6df72a6fb4d2)
- [Github Repository - TemporaryFile Class Implementation](https://github.com/laceyhenschel/TemporaryFile)