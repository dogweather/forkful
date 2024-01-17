---
title:                "Erstellen einer temporären Datei"
html_title:           "Swift: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Erstellen einer temporären Datei ist eine gängige Aufgabe für Programmierer. Es bezieht sich auf das Erstellen einer Datei, die nur für den aktuellen Programmlauf existiert und danach automatisch gelöscht wird. Dies kann nützlich sein, um temporäre Daten zu speichern, die während der Programmausführung benötigt werden.

# Wie geht's?
Um eine temporäre Datei in Swift zu erstellen, können wir die `FileManager`-Klasse verwenden. Das folgende Beispiel zeigt, wie wir eine Datei mit dem Namen "temp.txt" im temporären Verzeichnis erstellen und Text in die Datei schreiben können. Beachten Sie, dass wir eine `do-catch`-Anweisung verwenden, um mögliche Fehler beim Erstellen der Datei zu behandeln.

```Swift
let fileManager = FileManager.default
do {
    // Erstelle eine temporäre Datei namens "temp.txt"
    let tempDirURL = URL(fileURLWithPath: NSTemporaryDirectory())
    let tempFileURL = tempDirURL.appendingPathComponent("temp.txt")
    
    // Schreibe Text in die Datei
    let text = "Dies ist ein Beispieltext."
    try text.write(to: tempFileURL, atomically: true, encoding: .utf8)
    
    // Lese den Inhalt der Datei aus und gib ihn aus
    let fileContents = try String(contentsOf: tempFileURL, encoding: .utf8)
    print(fileContents)
} catch {
    print("Fehler beim Erstellen der temporären Datei: \(error)")
}
```

Die Ausgabe dieses Codes sollte "Dies ist ein Beispieltext." sein.

# Tief einsteigen
Das Erstellen temporärer Dateien war in früheren Versionen von Swift aufgrund des Fehlens von APIs wie `NSTemporaryDirectory()` und `FileManager.default` etwas komplizierter. Außerdem gab es keine Möglichkeit, die temporäre Datei automatisch zu löschen, wodurch möglicherweise Speicherplatz verschwendet wurde.

Eine Alternative zum Erstellen und Verwalten temporärer Dateien ist die Verwendung von In-Memory-Strukturen wie Arrays und Dictionaries. Diese haben jedoch den Nachteil, dass sie den verfügbaren Speicher belasten können und nicht so effizient sind wie das Schreiben in eine Datei.

Die Implementierung des `FileManager`-Frameworks basiert auf dem C-API `POSIX`, das für das Betriebssystem darunter verantwortlich ist, die Datei- und Verzeichnisoperationen durchzuführen. Dies bedeutet, dass das Erstellen temporärer Dateien in Swift auf einer höheren Ebene auf demselben Konzept aufbaut.

# Siehe auch
- Dokumentation von Apple zum Erstellen temporärer Dateien in Swift: https://developer.apple.com/documentation/foundation/filemanager
- ABSave - Eine Open-Source-Bibliothek, die das Speichern von Dateien und Objekten vereinfacht: https://github.com/ABTeam/ABSave