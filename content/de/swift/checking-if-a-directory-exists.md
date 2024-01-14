---
title:    "Swift: Überprüfung der Existenz eines Verzeichnisses"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Manchmal müssen wir in unserer Swift-Programmierung überprüfen, ob ein bestimmter Verzeichnispfad existiert. Dies kann aus verschiedenen Gründen geschehen, wie zum Beispiel das Lesen oder Schreiben von Dateien oder das Erstellen von Verzeichnissen.

## Wie man es macht

Um zu überprüfen, ob ein Verzeichnis existiert, müssen wir die `FileManager`-Klasse verwenden. Diese Klasse bietet standardmäßig die Funktion `fileExists(atPath:)`, die uns genau das liefert, wonach wir suchen. Hier ist ein Beispielcode, um dies zu demonstrieren:

```Swift
let fileManager = FileManager.default
let documentURL = fileManager.urls(for: .documentDirectory, in: .userDomainMask).first! //Pfad zum Dokumentenverzeichnis erhalten
let directoryExists = fileManager.fileExists(atPath: documentURL.path) //Überprüfen, ob das Dokumentenverzeichnis existiert

print(directoryExists) //Output: true
```

In diesem Beispiel verwenden wir den `FileManager`, um den Pfad zum Dokumentenverzeichnis zu erhalten und dann die Funktion `fileExists(atPath:)` aufzurufen, um zu überprüfen, ob dieses Verzeichnis existiert. Der Rückgabewert ist eine boolesche Variable, die `true` ist, wenn sie existiert und `false`, wenn nicht.

## Tiefer Einblick

Unter der Haube verwendet `fileExists(atPath:)` die `stat()` C-Funktion, um den Dateistatus abzurufen. Wenn der Pfad gültig ist, wird ein nicht negativer Wert zurückgegeben, was bedeutet, dass die Datei existiert. Andernfalls gibt es einen negativen Wert zurück, was bedeutet, dass die Datei nicht gefunden wurde. Diese Funktion ist sehr effizient und schnell, da sie nicht versucht, die Datei tatsächlich zu öffnen oder zu lesen, sondern nur den Status abruft.

## Siehe auch

- [Apple Dokumentation über die `FileManager`-Klasse](https://developer.apple.com/documentation/foundation/filemanager)
- [Apple Dokumentation über die `fileExists(atPath:)`-Funktion](https://developer.apple.com/documentation/foundation/filemanager/1410696-fileexists)
- [Tutorial zum Arbeiten mit Dateien und Verzeichnissen in Swift](https://www.raywenderlich.com/4677585-beginning-ios-file-management-in-swift-part-1-2)