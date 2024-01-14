---
title:                "Swift: Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

In der heutigen digitalen Welt werden immer mehr Daten gespeichert und verwaltet. Daher ist es wichtig zu wissen, ob ein bestimmter Ordner vorhanden ist, bevor man versucht, auf ihn zuzugreifen. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man in Swift überprüfen kann, ob ein Verzeichnis vorhanden ist.

## Wie geht's

Um zu prüfen, ob ein Verzeichnis vorhanden ist, können wir die Funktion ```fileExists(atPath:)``` aus der Klasse ```FileManager``` verwenden. Diese Funktion gibt einen booleschen Wert zurück, der angibt, ob der angegebene Pfad tatsächlich ein Verzeichnis ist oder nicht. Hier ist ein Beispielcode:

```
let fileManager = FileManager.default
let path = "/Users/username/Documents"

if fileManager.fileExists(atPath: path) {
    print("Das Verzeichnis existiert.")
} else {
    print("Das Verzeichnis existiert nicht.")
}
```

Wenn das Verzeichnis vorhanden ist, wird die Ausgabe "Das Verzeichnis existiert." sein. Andernfalls wird "Das Verzeichnis existiert nicht." ausgegeben.

## Tiefergehende Analyse

Es kann vorkommen, dass wir nicht nur wissen wollen, ob ein Verzeichnis existiert, sondern auch weitere Informationen darüber benötigen. Zum Beispiel möchten wir vielleicht wissen, ob das Verzeichnis lesbaren Inhalt hat oder ob es schreibgeschützt ist. Hier kommt die Funktion ```attributesOfItem(atPath:)``` ins Spiel. Diese Funktion gibt ein Dictionary mit verschiedenen Eigenschaften des angegebenen Pfades zurück, einschließlich der Attribute des Verzeichnisses. Hier ist ein erweitertes Beispiel:

```
let fileManager = FileManager.default
let path = "/Users/username/Documents"

if let attributes = try? fileManager.attributesOfItem(atPath: path) {
    print("Das Verzeichnis ist vom Typ: \(attributes[FileAttributeKey.type])")
    print("Das Verzeichnis hat Inhalt der Größe: \(attributes[FileAttributeKey.size])")
    print("Das Verzeichnis ist schreibgeschützt: \(attributes[FileAttributeKey.immutable])")
} else {
    print("Das Verzeichnis existiert nicht.")
}
```

Die Ausgabe wird eine Liste von Eigenschaften des Verzeichnisses ausgeben, wie zum Beispiel sein Typ, die Größe des Inhalts und ob es schreibgeschützt ist.

## Siehe auch

- [Apple Dokumentation über das ```FileManager```-Modul](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial über das Lesen und Schreiben von Dateien in Swift](https://www.appcoda.com/swift4-filesystem-api/)

Bitte denke daran, dass es immer wichtig ist, sicherzustellen, dass die richtigen Berechtigungen vorhanden sind, um auf ein Verzeichnis oder eine Datei zuzugreifen, bevor man den Code ausführt. In der Regel solltest du dich auf das Prüfen des Vorhandenseins von Verzeichnissen beschränken, da das Überprüfen der Eigenschaften des Verzeichnisses mehr Aufwand und Verarbeitung erfordern kann. Wir hoffen, dass dieser Beitrag dir geholfen hat, mehr über das Überprüfen von Verzeichnissen in Swift zu erfahren. Bis zum nächsten Mal!