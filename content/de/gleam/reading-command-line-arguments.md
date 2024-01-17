---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Gleam: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Was & Warum?

Das Lesen von Befehlszeilenargumenten ist eine gängige Aufgabe für Programmierer. Es bedeutet, dass das Programm beim Start Informationen aus der Befehlszeile erhält, die es dann verarbeiten kann. Dies ist besonders nützlich, wenn das Programm auf unterschiedlichen Eingaben basieren oder konfiguriert werden soll.

# Wie geht's?

In Gleam kann dies mit der Funktion `command_args` erreicht werden. Diese nimmt keine Argumente entgegen und gibt ein Array von Strings zurück, die die Befehlszeilenargumente enthalten. Hier ist ein Beispiel:

``` Gleam
fn main() {
  let args = command_args()
  io.println("Befehlszeilenargumente:")
  for arg in args {
    io.println(arg)
  }
}
```

Wenn wir dieses Programm mit dem folgenden Befehl ausführen:
```
$ gleam run example.gleam arg1 arg2
```
Lautet die Ausgabe:
```
Befehlszeilenargumente:
arg1
arg2
```

# Tiefere Einblicke

Das Lesen von Befehlszeilenargumenten ist keine neue Aufgabe und wird von vielen Sprachen unterstützt. Alternativ können auch Umgebungsvariablen oder Konfigurationsdateien verwendet werden, um Programme zu konfigurieren. Das Lesen von Befehlszeilenargumenten ist oft schneller und einfacher, da keine externe Datei oder Variablen erstellt werden müssen. In Gleam werden Befehlszeilenargumente durch die Standardbibliothek `gleam/io` verarbeitet.

# Siehe auch

Weitere Informationen zur Verwendung von Befehlszeilenargumenten in Gleam finden Sie in der offiziellen Dokumentation der Standardbibliothek: https://gleam.run/std/io#command-args.