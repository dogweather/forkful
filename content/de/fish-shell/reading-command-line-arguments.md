---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Fish Shell: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn du ein erfahrener Programmierer bist oder einfach nur ein Interesse an der Kommandozeile hast, ist es wichtig zu wissen, wie man Argumente in der Fish Shell liest. Dies hilft dir, effektiver und schneller zu arbeiten, da du damit deine Skripte und Befehle anpassen und automatisieren kannst.

## Wie geht's?

Um Argumente in der Fish Shell zu lesen, kannst du das `argparse`-Modul verwenden, das bereits in Fish integriert ist. Hier ist ein Beispielcode, der alle Argumente, die beim Aufruf des Skripts übergeben werden, ausgibt:

```Fish Shell
#!/usr/bin/env fish

use argparse

function parse_arguments -d "Parse command line arguments"
    set -l parser (argparse -p "My Script" -d "This is my script")
    argparse::add_argument $parser "argument"
    argparse::parse $parser \$argv
    echo (argparse::get_argument $parser "argument")
end

parse_arguments
```

Angenommen, das obige Skript wurde als `myscript.fish` gespeichert, hier sind einige Beispiele für die Ausgabe, je nachdem, welche Argumente übergeben wurden:

```
$ ./myscript.fish hello
Hello

$ ./myscript.fish -h
My Script
This is my script

$ ./myscript.fish --help
My Script
This is my script
```

## Tief tauchen

Das `argparse`-Modul bietet ein umfangreiches Set an Funktionen, mit denen du die Argumente auswerten und verwenden kannst. Hier sind einige Beispiele für weitere Funktionen, die du verwenden kannst:

- `argparse::add_argument` - Fügt ein Argument mit einem angegebenen Namen hinzu.
- `argparse::add_option` - Fügt eine Option mit einem angegebenen Namen hinzu.
- `argparse::add_subparser` - Fügt einen Subparser hinzu, um verschiedene Befehle in einem Skript zu verarbeiten.
- `argparse::parse` - Verarbeitet die übergebenen Argumente und speichert sie in einem Parserobjekt.
- `argparse::get_argument` - Ruft den Wert eines bestimmten Arguments aus dem Parserobjekt ab.

Für eine vollständige Liste aller Funktionen und weitere Beispiele kannst du die offizielle Dokumentation des Fish-Teams unter [https://fishshell.com/docs/current/index.html#argparse](https://fishshell.com/docs/current/index.html#argparse) besuchen.

## Siehe auch

- [Die offizielle Fish Shell-Dokumentation](https://fishshell.com/docs/current/)
- [Eine Einführung in die Fish Shell für Einsteiger](https://linuxhint.com/getting-started-with-fish-shell/)