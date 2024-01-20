---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Einlesen einer Textdatei ist der Prozess, bei dem Daten aus einer .txt-Datei in den Computer geladen werden. Programmierer machen dies, um Daten zu manipulieren, im Speicher zu speichern und auszuwerten.

## So geht es:

Hier ist ein einfacher Weg, wie man eine Textdatei in Swift lesen kann. Führen Sie den folgenden Code aus:

```Swift 
import Foundation

do {
    // Der Pfad zur Datei
    let path = "/Pfad/Zu/Deiner/Datei.txt"

    // Versuche die Datei von diesem Pfad einzulesen
    let textfile = try String(contentsOfFile: path, encoding: .utf8)

    print(textfile)
} catch {
     // Wenn ein Fehler auftritt, printe den Fehler
     print("Ein Fehler ist aufgetreten: \(error)")
}
```

Wenn Sie diesen Code ausführen und Ihr Datei existiert, wird der Text aus der Datei in der Konsole ausgegeben.

## Tiefer Eintauchen:

Dateien wurden schon immer in der Programmierung gelesen. Diese Praxis entstand aus der Notwendigkeit, Daten einfach zu speichern und wieder abzurufen. Es gibt viele Alternativen zum Lesen von Textdateien in Swift, einschließlich der Nutzung von Bibliotheken wie FileReader und FileInputStream.

Was die Implementierungsdetails angeht, verwendet die Funktion `contentsOfFile` die von der `NSString` Klasse bereitgestellten Methoden zum Arbeiten mit unveränderten Unicode-Zeichensequenzen. Die Funktion `try` ist in einem `do-catch` Block eingebettet, um mögliche Fehler beim Einlesen der Datei zu behandeln.

## Siehe Auch:

Für weitere Informationen und fortgeschrittene Nutzungsfälle, schauen Sie sich bitte die folgenden Ressourcen an:

1. Apple's Swift-Dokumentation: [https://developer.apple.com/documentation/swift](https://developer.apple.com/documentation/swift)
2. FileReader Bibliothek: [https://github.com/vincent/fileReader-Swift](https://github.com/vincent/fileReader-Swift)
3. FileInputStream Guide: [https://www.hackingwithswift.com/example-code/system/how-to-read-the-contents-of-a-directory-using-filesystem](https://www.hackingwithswift.com/example-code/system/how-to-read-the-contents-of-a-directory-using-filesystem)