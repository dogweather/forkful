---
title:    "Swift: Lesen von Befehlszeilenargumenten"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/swift/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum
Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit, die jeder Swift-Programmierer beherrschen sollte. Mit diesem Wissen kannst du deinen Code flexibler gestalten und ihn an verschiedene Eingaben anpassen. In diesem Blogbeitrag erfährst du, wie du Befehlszeilenargumente in Swift lesen und verarbeiten kannst.

## Wie man Befehlszeilenargumente liest
Um Befehlszeilenargumente in Swift zu lesen, kannst du die `CommandLine`-Klasse verwenden. Hier ist ein Beispiel, wie du die Argumente aus der Eingabeaufforderung liest und in einer Schleife ausgibst:

```Swift
let args = CommandLine.arguments
for arg in args{
    print(arg)
}
```

Die Ausgabe wird alle eingegebenen Argumente in der Reihenfolge, in der sie eingegeben wurden, ausgeben. Zum Beispiel, wenn du dein Programm mit dem Befehl `swift argumentExample.swift argument1 argument2` aufrufst, wird die Ausgabe folgendermaßen aussehen:

```
argumentExample.swift
argument1
argument2
```

Du kannst auch überprüfen, ob bestimmte Argumente vorhanden sind und darauf reagieren. Hier ist ein Beispiel, wie du herausfinden kannst, ob das Argument "-h" eingegeben wurde und entsprechend reagieren kannst:

```Swift
let args = CommandLine.arguments
if args.contains("-h"){
    print("Hilfe anzeigen")
}
```

## Tiefer Einblick
Die `CommandLine`-Klasse bietet neben dem Lesen von Argumenten noch weitere nützliche Funktionen. Du kannst zum Beispiel auch den Namen des Programms oder die Anzahl der eingegebenen Argumente abrufen. Hier sind einige nützliche Eigenschaften und Methoden der `CommandLine`-Klasse:

- `arguments`: gibt ein Array von String-Argumenten zurück
- `commandName`: gibt den Namen des ausgeführten Programms zurück
- `argumentCount`: gibt die Anzahl der eingegebenen Argumente zurück
- `print()` oder `write()`: gibt Text auf der Konsole aus oder schreibt ihn in eine Datei
- `error`: ermöglicht das Schreiben von Fehlern auf die Konsole oder in eine Datei

Wenn du tiefer in die `CommandLine`-Klasse einsteigen möchtest, empfehle ich dir, die offizielle Dokumentation von Apple zu lesen.

## Siehe auch
- [Offizielle Dokumentation der `CommandLine`-Klasse von Apple](https://developer.apple.com/documentation/foundation/commandline)
- [Tutorial zum Lesen von Befehlszeilenargumenten in Swift](https://www.swiftbysundell.com/basics/command-line-arguments/)