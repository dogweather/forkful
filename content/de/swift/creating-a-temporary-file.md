---
title:                "Erstellung einer temporären Datei"
html_title:           "Swift: Erstellung einer temporären Datei"
simple_title:         "Erstellung einer temporären Datei"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich damit beschäftigen, eine temporäre Datei zu erstellen? Nun, es gibt viele Situationen, in denen man vorübergehend eine Datei benötigt, um Daten zu speichern oder zu verarbeiten. Zum Beispiel, wenn man eine App entwickelt, kann es hilfreich sein, temporäre Dateien zu nutzen, um Zwischenergebnisse zu speichern oder um Daten zwischen verschiedenen Teilen der App auszutauschen.

## Wie

Das Erstellen einer temporären Datei in Swift ist eigentlich ziemlich einfach. Man kann die `FileManager`-Klasse verwenden, um einen temporären Dateipfad zu generieren und dann eine Datei an diesem Pfad zu erstellen. Hier ist ein Beispiel:

```Swift
let fileManager = FileManager.default
let tempDirURL = URL(fileURLWithPath: NSTemporaryDirectory())
let tempFileURL = tempDirURL.appendingPathComponent("tempFile.txt")

do {
    try "Lorem ipsum dolor sit amet".write(to: tempFileURL, atomically: true, encoding: .utf8)
    print("Temporäre Datei erfolgreich erstellt!")
} catch {
    print(error)
}
```

In diesem Beispiel verwenden wir den `NSTemporaryDirectory()`-Pfad, um den temporären Ordner des Betriebssystems zu erhalten. Dann fügen wir den Namen der temporären Datei an dieses URL-Objekt an und können dann die `write(to:atomically:encoding:)`-Methode nutzen, um Daten in die Datei zu schreiben. Natürlich kann man den Inhalt und den Dateinamen anpassen, je nach Bedarf.

Wir können auch die `isTemporary`-Eigenschaft des `URL`-Objekts nutzen, um sicherzustellen, dass es sich um eine temporäre Datei handelt, falls dies wichtig ist.

## Deep Dive

Man könnte sich fragen, was genau eine temporäre Datei von einer "normalen" Datei unterscheidet. Nun, temporäre Dateien sind in der Regel dazu gedacht, nur für kurze Zeit zu existieren und dann gelöscht zu werden. Sie bieten eine einfache Möglichkeit, temporäre Daten zu speichern, ohne dass man sich um die Datenverwaltung kümmern muss.

Zusätzlich werden temporäre Dateien normalerweise in einem temporären Verzeichnis gespeichert, das vom Betriebssystem verwaltet wird. Dies kann hilfreich sein, um Speicherplatz auf dem Gerät zu sparen oder um sicherzustellen, dass die Dateien automatisch gelöscht werden, wenn sie nicht mehr benötigt werden.

Man sollte jedoch beachten, dass temporäre Dateien auch Nachteile haben können. Sie können zum Beispiel ungewollt gelöscht werden, wenn das Betriebssystem den temporären Ordner bereinigt oder wenn die App abstürzt. Daher sollte man immer sicherstellen, dass man seine temporären Dateien regelmäßig überprüft und löscht, wenn sie nicht mehr benötigt werden.

## Siehe auch

* [Apple Dokumentation zur `FileManager`-Klasse](https://developer.apple.com/documentation/foundation/filemanager)
* [Tutorial zur Verwendung von `NSTemporaryDirectory()`](https://www.hackingwithswift.com/read/16/4/the-ultimate-guide-to-nsfilemanager)
* [Diskussion über Vor- und Nachteile von temporären Dateien](https://stackoverflow.com/questions/3448603/any-advantages-to-using-temporary-files-vs-in-memory-arrays)