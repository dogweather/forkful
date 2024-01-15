---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Swift: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn du ein fortgeschrittener Swift-Entwickler bist, möchtest du vielleicht lernen, wie man Befehlszeilenargumente in deine Programme einbinden kann. Mit diesem Wissen kannst du beispielsweise eine eigene Kommandozeilenanwendung erstellen oder vorhandene Programme erweitern.

## Wie geht das

Es gibt verschiedene Möglichkeiten, Befehlszeilenargumente in Swift zu lesen. Die einfachste Methode ist die Verwendung von `CommandLine.arguments`, das eine Array von Strings zurückgibt. In diesem Beispiel werden wir eine Eingabeaufforderung mit zwei Argumenten erstellen und die Argumente in der Konsole ausgeben:

```
Swift
let args = CommandLine.arguments
print("Das erste Argument ist: \(args[1])")
print("Das zweite Argument ist: \(args[2])")
```

Wenn du deine Anwendung über die Befehlszeile ausführst und zwei Argumente angegeben hast, wirst du folgende Ausgabe sehen:

```
Swift
$ ./meinprogramm argument1 argument2
Das erste Argument ist: argument1
Das zweite Argument ist: argument2
```

Du kannst auch überprüfen, ob bestimmte Argumente vorhanden sind und entsprechende Aktionen ausführen. Zum Beispiel könntest du deine Anwendung so gestalten, dass sie ein bestimmtes Verhalten zeigt, wenn das Argument `--help` angegeben wird. Hier ist ein Beispielcode:

```
Swift
let args = CommandLine.arguments
if args.contains("--help") {
    print("Dies ist die Hilfeseite für mein Programm.")
    print("Gib `--version` ein, um die Versionsnummer anzuzeigen.")
}
```

## Tiefere Einblicke

Manchmal möchtest du vielleicht mehr Kontrolle über die Befehlszeilenargumente haben, z.B. um bestimmte Argumente in spezifische Datentypen zu konvertieren. Dafür gibt es die Klasse `CommandLineOption`. Hier ein Beispiel:

```
Swift
let options = CommandLineOption.arguments([
    .option("input", .single, default: nil),
    .option("output", .single, default: "output.txt")
])

guard let input = options["input"] else {
    print("Es muss ein Eingabedokument angegeben werden.")
    exit(0)
}

let output = options["output"]

print("Das Eingabedokument ist: \(input)")
print("Das Ausgabedokument ist: \(output)")
```

In diesem Beispiel wird überprüft, ob das Argument `input` vorhanden ist und dann als String ausgegeben. Wenn das Argument `output` nicht angegeben wird, wird standardmäßig "output.txt" ausgegeben. Du kannst auch verschiedene Werte für ein Argument zulassen, z.B. durch Hinzufügen von `.multiple` nach dem Argumentnamen.

## Siehe auch

- [Apple Dokumentation zu Befehlszeilenparametern in Swift](https://developer.apple.com/documentation/swift/command-line_arguments)
- [Swift ArgParser - Eine Bibliothek zur Analyse von Befehlszeilenargumenten](https://github.com/apple/swift-argument-parser)