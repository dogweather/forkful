---
title:                "Swift: Das Lesen von Befehlszeilenargumenten"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man sich mit der Verwendung von Befehlszeilenargumenten in Swift auseinandersetzen sollte. Zum einen ist es eine wichtige Fähigkeit, die in vielen Projekten und Anwendungen benötigt wird. Zum anderen ermöglicht es eine noch flexiblere und individuellere Programmierung. In diesem Blogbeitrag werden wir uns damit beschäftigen, wie man Befehlszeilenargumente in Swift einlesen kann und welche Möglichkeiten sich dadurch eröffnen.

## Wie geht man vor?

Zuerst muss man die Befehlszeilenargumente als Array von Strings einlesen. Dies geschieht mit der Funktion `CommandLine.arguments`. Anschließend kann man auf die einzelnen Argumente zugreifen und diese in eine für das Programm sinnvolle Form bringen. Hier ein Beispielcode:

```Swift
let arguments = CommandLine.arguments
print(arguments)

// Output: ["programName", "argument1", "argument2", "argument3"]
```

Man kann auch gezielt auf ein bestimmtes Argument zugreifen, indem man den Index angibt. Wichtig ist dabei zu beachten, dass der erste Index (0) immer der Name des Programms ist. Hier ein Beispiel:

```Swift
let argument2 = arguments[2]
print(argument2)

// Output: "argument2"
```

Man kann außerdem auch überprüfen, ob bestimmte Optionen oder Flags gesetzt wurden, indem man die Funktion `contains` verwendet. Hier ein Beispiel:

```Swift
if arguments.contains("-f") {
    // Code, der ausgeführt wird, wenn das Argument -f gesetzt wurde
}
```

## Deep Dive

Eine weitere wichtige Funktion beim Lesen von Befehlszeilenargumenten ist `CommandLine.Option`. Diese ermöglicht es, gezielt nach bestimmten Argumenten zu suchen und diese in einer sinnvollen Form zu verwenden. Hier ein Beispiel:

```Swift
let fileOption = Option<String>("-f", "--file")
let filename = fileOption! // Der Optionenwert ist optional, daher muss man ihn auspacken
print(filename)

// Beispielaufruf mit "-f test.txt" als Argument: "test.txt"
```

Es ist außerdem möglich, Optionen mit einem Standardwert zu versehen, falls kein Argument angegeben wurde. Hier ein Beispiel:

```Swift
let logOption = Option<String>("-l", "--log", default: "output.log")
let logfile = filename ?? "kein Logfile angegeben" // Falls kein Argument angegeben wurde, nimmt es den Standardwert "output.log"
print(logfile)
```

Zusätzlich gibt es noch weitere Funktionen und Optionen, welche das Lesen von Befehlszeilenargumenten noch flexibler und benutzerfreundlicher machen. Hierzu sollte man sich mit der Dokumentation von Swift auseinandersetzen und experimentieren.

## Siehe auch

- [Swift Dokumentation - Command Line Arguments](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID522)
- [Technisches Tutorial - Introduction to Command Line Arguments in Swift](https://medium.com/@jamesrochabrun/fun-with-command-line-arguments-in-swift-9f6ba12dc201)
- [Code-Beispiel - Handling Command Line Arguments in Swift](https://gist.github.com/dstalzer/23196be4b101ca18b9b6)