---
title:                "Bash: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum?

Beim Programmieren in Bash kann es oft sinnvoll sein, temporäre Dateien zu erstellen. Diese Dateien dienen als "Arbeitsbereich" für Skripte und können für verschiedene Zwecke verwendet werden, wie zum Beispiel zum Speichern von Zwischenergebnissen oder temporären Variablen. Das Erstellen einer temporären Datei kann auch dazu beitragen, Speicherplatz und Ressourcen auf dem System zu sparen, da sie automatisch gelöscht wird, wenn das Skript beendet wird.

## Wie erstellt man eine temporäre Datei in Bash?

Es gibt mehrere Möglichkeiten, eine temporäre Datei in Bash zu erstellen. Eine gängige Methode ist die Verwendung des `mktemp` Befehls. Dieser Befehl erstellt eine eindeutige temporäre Datei und gibt ihren Dateinamen aus, den Sie dann in Ihrem Skript verwenden können.

Ein Beispiel für die Verwendung von `mktemp`:

```Bash
temp_file=$(mktemp)
echo "Dies ist eine temporäre Datei." > $temp_file
cat $temp_file
```

Dieses Skript erstellt eine temporäre Datei mit dem Namen, der vom `mktemp` Befehl ausgegeben wird. Dann wird der Text "Dies ist eine temporäre Datei." in diese Datei geschrieben und mit dem `cat` Befehl gelesen. Denken Sie daran, am Ende Ihres Skripts die temporäre Datei zu löschen, um Ressourcen zu sparen.

## Tiefere Einblicke

Es gibt verschiedene Möglichkeiten, eine temporäre Datei in Bash zu erstellen, je nach Ihren spezifischen Anforderungen. Sie können zum Beispiel die Option `-p` verwenden, um den Ort der temporären Datei anzugeben, oder die Option `-d` für das angegebene Verzeichnis, in dem die Datei erstellt werden soll.

Darüber hinaus können Sie mit dem `mktemp` Befehl auch bestimmte Dateinamenmuster festlegen oder eine Zufallszahl in den Dateinamen einfügen, um die Einzigartigkeit der temporären Datei zu gewährleisten.

Es ist auch wichtig zu beachten, dass die Erstellung einer temporären Datei allein nicht immer ausreichend ist. Sie müssen möglicherweise auch die Zugriffsrechte der Datei anpassen, um sicherzustellen, dass nur autorisierte Benutzer darauf zugreifen können.

## Siehe auch

- [Die `mktemp`-Manpage](https://manpages.debian.org/stretch/tmpreaper/mktemp.1.en.html)
- [Shell-Skript in 30 Minuten - Kapitel 12](https://www.linux.com/training-tutorials/how-write-simple-bash-shell-script/)
- [StackOverflow-Fragen zur Verwendung von `mktemp` in Bash](https://stackoverflow.com/questions/tagged/mktemp+bash)