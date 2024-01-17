---
title:                "Eine Textdatei lesen"
html_title:           "Fish Shell: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Textdateien ist ein wesentlicher Bestandteil der Programmierung. Es ermöglicht Programmierern, Daten aus externen Quellen zu lesen und in ihren Code zu integrieren. Dadurch können sie dynamische Anwendungen erstellen, die Benutzereingaben verarbeiten oder Informationen von anderen Programmen abrufen.

## Anleitung:
Das Lesen einer Textdatei in der Fish Shell ist einfach und unkompliziert. Hier ein Beispiel:

```fish
set file (cat test.txt)
echo $file
```

Die erste Zeile liest die Datei "test.txt" und speichert sie in der Variablen "file". In der zweiten Zeile wird der Inhalt dieser Variable ausgegeben. Hier ist das erwartete Ergebnis:

```
Dies ist ein Beispieltext.
Er enthält verschiedene Zeilen und Zeichen.
```

## Tiefsee:
Die Möglichkeit, Textdateien zu lesen, gibt es schon seit den Anfängen der Programmierung. Das Lesen von Textdateien ist auch in anderen Shells wie Bash oder Zsh möglich, aber die Syntax unterscheidet sich leicht.

Es gibt auch andere Möglichkeiten, Daten aus externen Quellen zu lesen, wie z.B. das Ausführen von Befehlen mit der "exec" Funktion oder das Einlesen von Daten aus dem Internet mit dem Curl-Befehl.

Das Lesen von Textdateien in Fish Shell basiert auf der "cat" Funktion, die die Datei Zeile für Zeile durchläuft und deren Inhalt ausgibt.

## Siehe auch:
- `cat` Funktion in der Fish Shell [https://fishshell.com/docs/current/cmds/cat.html]
- Vergleich der verschiedenen Shells [https://blog.teamtreehouse.com/the-versatility-of-shells]