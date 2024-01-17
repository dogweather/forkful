---
title:                "Durch die Lese von Befehlszeilenargumenten"
html_title:           "Swift: Durch die Lese von Befehlszeilenargumenten"
simple_title:         "Durch die Lese von Befehlszeilenargumenten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Befehlszeilenargumenten ist eine Möglichkeit, Eingaben zu einem Programm zu machen, die direkt in der Kommandozeile eingegeben werden können. Programmierer tun dies, um ihre Programme flexibler zu gestalten und es Benutzern zu ermöglichen, unterschiedliche Eingaben zu machen, ohne den Code jedes Mal ändern zu müssen.

## Wie geht's?

Swift bietet die Möglichkeit, Befehlszeilenargumente mit Hilfe der ```CommandLine``` Klasse zu lesen. Hier ist ein Beispiel, wie man das machen kann:

```Swift
let arguments = CommandLine.arguments
print("Der erste eingegebene Wert ist: \(arguments[1])")
```

Dieses Beispiel nimmt den zweiten Wert, der in der Kommandozeile eingegeben wird, und gibt ihn auf der Konsole aus. Wenn der Befehl also wie folgt eingegeben wird:

```bash
$ SwiftProgramm Argument1 Argument2
```

wird auf der Konsole ```Argument2``` ausgegeben.

## Tiefer Einblick

Das Lesen von Befehlszeilenargumenten hat eine lange Geschichte in der Programmierung und wird von Programmierern auf der ganzen Welt verwendet. Eine alternative Möglichkeit, Eingaben zu einem Programm zu machen, ist die Erstellung einer grafischen Benutzeroberfläche (GUI), die dem Benutzer Eingabefelder zur Verfügung stellt. Die Verwendung von Befehlszeilenargumenten ist jedoch oft bevorzugt, da dies eine schnellere und effizientere Methode ist.

Die CommandLine Klasse bietet auch die Möglichkeit, auf spezielle Argumente wie ```-h``` oder ```--help``` zu reagieren, die in der Regel dafür verwendet werden, um dem Benutzer eine Hilfe- oder Dokumentationsanweisung anzuzeigen. Dies kann es für Benutzer einfacher machen, Ihr Programm zu verstehen und zu navigieren.

## Siehe auch

Hier sind einige nützliche Quellen, um mehr über die ```CommandLine``` Klasse und das Lesen von Befehlszeilenargumenten in Swift zu erfahren:

- [Apple Dokumentation zur CommandLine Klasse](https://developer.apple.com/documentation/foundation/commandline)
- [Tutorial zum Lesen von Befehlszeilenargumenten in Swift](https://www.hackingwithswift.com/example-code/system/how-to-read-command-line-arguments-using-commandline)
- [Stack Overflow Artikel über die Verwendung von Befehlszeilenargumenten in Swift](https://stackoverflow.com/questions/24045528/swift-how-to-pass-command-line-arguments-to-an-command-line-application)