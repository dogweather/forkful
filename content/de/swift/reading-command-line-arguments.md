---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lesen der Befehlszeilenargumente in Swift

## Was & Warum?
Befehlszeilenargumente sind Werte, die Sie an Ihr Programm beim Start über die Befehlszeile übergeben. Sie geben Programmierern die Möglichkeit, das Verhalten des Programms zu ändern, ohne den Code anpassen zu müssen.

## So geht's:

Sie können auf Befehlszeilenargumente in Swift über das `CommandLine` Verzeichnis zugreifen. Hier ist ein Beispielcode:

```Swift
let argumente = CommandLine.arguments

for argument in argumente {
    print("Argument: \(argument)")
}
```

Wenn Sie dieses Programm mit `swift program.swift arg1 arg2` ausführen, sehen Sie folgende Ausgabe:

```Swift
Argument: program.swift
Argument: arg1
Argument: arg2
```

## Vertiefung

Historisch gesehen stammen Befehlszeilenargumente aus der Zeit der ersten Betriebssysteme und Sprachen wie C und Bash. In Swift haben wir Zugang zum `CommandLine.arguments` Array. Da Swift auf halbem Wege zwischen Hoch- und Niedrigsprachen liegt, gibt es keine eingebauten Hilfsfunktionen zur Verarbeitung der Argumente. 

Es gibt jedoch Alternativen. Frameworks wie Swift Argument Parser bieten detailliertere Funktionen für Befehlszeilenargumente. Sie können die Argumente als Typen behandeln und sogar automatisierte Hilfe und Fehlermeldungen erzeugen.

## Siehe auch

- SWIFT CommandLine.Argument: [Apple Swift CommandLine.Argument](https://developer.apple.com/documentation/swift/commandline/2876009-arguments)
- Swift Argument Parser: [Swift Argument Parser auf Github](https://github.com/apple/swift-argument-parser)
- Blog über die Verwendung des Swift Argument Parser: [Swift Argument Parser Tutorial](https://www.hackingwithswift.com/articles/216/complete-guide-to-swift-argumentparser)