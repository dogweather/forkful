---
title:                "Gleam: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum
Manche Programmierer mögen es vielleicht nicht, aber die Wahrheit ist, dass das Lesen von Befehlszeilenargumenten eine wichtige Fähigkeit ist, die jeder Entwickler beherrschen sollte. Egal ob du ein einfaches Skript schreibst oder eine komplexe Anwendung entwickelst, das Lesen von Befehlszeilenargumenten kann dir helfen, interaktive Funktionen zu erstellen und deinen Code noch leistungsstärker zu machen.

## Wie Funktioniert Das
Um Befehlszeilenargumente in Gleam zu lesen, verwenden wir die `arg`-Bibliothek. Diese stellt uns eine Funktion zur Verfügung, die das Lesen von Argumenten vereinfacht. Schau dir das folgende Beispiel an:

```Gleam
// Definiere einige Argumente
let args = ["-u", "username", "-p", "password", "-d", "database"]

// Verwende die `arg`-Bibliothek, um diese Argumente zu lesen
let opts = arg.parse(args)

// Gib die gelesenen Werte aus
// Output: Some ["username", "password", "database"]
io.println(opts.map(x => x.value))
```

In diesem Beispiel haben wir eine Liste mit Befehlszeilenargumenten erstellt und diese dann mit der `parse`-Funktion gelesen. Das Ergebnis ist ein optionales Array mit den gelesenen Werten, welches wir dann einfach ausgeben können.

Natürlich können wir die `arg`-Bibliothek auch verwenden, um Argumente mit verschiedenen Datentypen wie z.B. Zahlen oder Bools zu lesen. Dazu können wir die Funktion `parse_as` verwenden, die uns ermöglicht, den gewünschten Typ der Argumente anzugeben.

```Gleam
// Definiere einige Argumente
let args = ["-p", "8080"]

// Verwende die `arg`-Bibliothek, um die Argumente als Integer zu lesen
let port = arg.parse_as(args, Int)

// Gib den gelesenen Wert aus
// Output: Some 8080
io.println(port.value)
```

## Tiefer Einblick
Die `arg`-Bibliothek ermöglicht es uns auch, optionale Argumente oder Argumente mit Standardwerten zu definieren. Dies ist besonders nützlich, wenn wir flexiblere Anwendungen entwickeln möchten.

```Gleam
// Definiere einige optionale Argumente
let args = ["-i", "input.txt", "-o", "output.txt"]

// Verwende die `arg`-Bibliothek, um die Argumente zu lesen
let opts = arg.parse(args)

// Definiere Default-Werte für die Argumente
let input = opts["i"]? |> Optional.with_default("input.txt")
let output = opts["o"]? |> Optional.with_default("output.txt")

// Gib die gelesenen Werte aus
// Output: Some ["input.txt", "output.txt"]
io.println(Some(tuple(input, output)))
```

Mit der `arg`-Bibliothek haben wir die Möglichkeit, auch komplexere Anwendungen zu entwickeln, die verschiedene Parameter und Einstellungen unterstützen.

## Siehe Auch
- [Die offizielle Gleam Dokumentation zur `arg`-Bibliothek](https://gleam.run/documentation/stdlib/arg)
- [Ein Tutorial zum Lesen von Befehlszeilenargumenten in Gleam](https://gleam.run/tutorials/cli-args)