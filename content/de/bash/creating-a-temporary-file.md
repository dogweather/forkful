---
title:                "Bash: Ein temporäres File erstellen."
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist ein wichtiger Teil der Bash-Programmierung. Es kann hilfreich sein, um Daten temporär zu speichern und zu verarbeiten, ohne die ursprüngliche Datei zu verändern. Außerdem kann es nützlich sein, um Skripte flexibler und wiederverwendbarer zu gestalten.

## Wie es geht

Die Erstellung einer temporären Datei in Bash ist relativ einfach und erfordert nur wenige Zeilen Code. Im Folgenden wird gezeigt, wie man eine temporäre Datei erstellt und schreibt:

```Bash
# Erstelle eine temporäre Datei
temp_file=$(mktemp)
# Schreibe Text in die Datei
echo "Das ist ein Beispieltext" > $temp_file
# Zeige den Inhalt der temporären Datei an
cat $temp_file
```

Die Ausgabe sollte folgendes anzeigen:

```
Das ist ein Beispieltext
```

## Tiefere Einblicke

In Bash gibt es verschiedene Möglichkeiten, um temporäre Dateien zu erstellen. Eine Möglichkeit ist die Verwendung von `mktemp`, wie im obigen Beispiel gezeigt. Dabei wird eine eindeutige Datei mit einer zufällig generierten Nummer erstellt. Es ist auch möglich, mit dem Befehl `tempfile` eine temporäre Datei zu erstellen, die jedoch nicht so flexibel ist wie `mktemp`.

Es ist auch wichtig zu beachten, dass temporäre Dateien standardmäßig im `/tmp` Verzeichnis erstellt werden. Dies kann jedoch geändert werden, indem man den `TMPDIR` Umgebungsvariablenwert auf ein anderes Verzeichnis setzt.

Das Löschen der temporären Datei sollte auch nicht vergessen werden, da sonst möglicherweise unerwünschte Dateien im System zurückbleiben. Um die erstellte temporäre Datei zu löschen, kann man einfach den beigefügten Variablennamen verwenden, beispielsweise:

```Bash
# Löschen der temporären Datei
rm $temp_file
```

## Siehe auch

-Die offizielle Bash-Dokumentation zu temporären Dateien
(http://www.gnu.org/software/bash/manual/html_node/Manipulating-Files.html)
- Ein Tutorial zum Erstellen und Löschen von temporären Dateien in Bash
(https://linuxize.com/post/bash-temporary-files/)
- Das Linux Manual zu `mktemp` (http://man7.org/linux/man-pages/man1/mktemp.1.html)