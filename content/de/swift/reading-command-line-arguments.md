---
title:                "Swift: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum
Eine der coolsten Funktionen von Swift ist die Möglichkeit, Befehlszeilenargumente zu lesen und damit interaktive und anpassbare Programme zu erstellen. Wenn du neugierig bist, wie du das machen kannst, bist du hier richtig!

## Wie man Befehlszeilenargumente in Swift liest
Befehlszeilenargumente sind Parameter, die beim Starten eines Programms in der Terminalumgebung übergeben werden können. In Swift kannst du diese Argumente einfach mit dem `CommandLine` Objekt auslesen.

```Swift
let arguments = CommandLine.arguments
```

Das gibt dir ein Array mit allen übergebenen Argumenten. Du kannst dann durch das Array iterieren und die Argumente verwenden, um verschiedene Funktionen innerhalb deines Programms auszuführen.

```Swift
for argument in arguments {
    if argument == "-h" {
        print("Dies ist eine Hilfe-Nachricht")
    } else if argument == "-v" {
        print("Version 1.0")
    }
}
```

Wenn du das Programm in deinem Terminal mit dem Befehl `swift Programm.swift -h` ausführst, wird die entsprechende Ausgabe auf dem Bildschirm angezeigt.

## Tiefergehender Einblick
Die `CommandLine` Klasse bietet auch weitere nützliche Funktionen, wie zum Beispiel `commandLine.arguments.count`, um die Anzahl der Argumente zu überprüfen, oder `commandLine.arguments.dropFirst()`, um das erste Argument (meistens der Dateiname) zu ignorieren.

Außerdem kannst du mit `CommandLine.arguments.joined(separator: " ")` die Argumente in einem String zusammenführen, der dann zum Beispiel für die Ausgabe in einer Datei oder per E-Mail verwendet werden könnte.

## Siehe auch
- [Offizielle Dokumentation von Apple zu Command-Line-Argumenten](https://developer.apple.com/documentation/foundation/commandline)
- [Swift Tutorial: Kontrollstrukturen und die `CommandLine` Klasse](https://www.ralfebert.de/ios/tutorials/swift_kontrollstrukturen/)
- [Eine Einführung in die iOS Entwicklung mit Swift](https://www.toptal.com/swift/ios-swift-tutorial-introduction-to-ios-apps)