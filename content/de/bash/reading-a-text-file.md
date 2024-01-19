---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen einer Textdatei in Bash bedeutet, den Inhalt der Datei Zeile für Zeile zu lesen und interpretieren. Dies ist nützlich für die Automatisierung von Aufgaben und Analyse von Dateien.

## So geht's:

Hier ist ein einfaches Bash-Script, um eine Datei zu lesen:

```Bash
while IFS= read -r line
do
    echo "$line"
done < "dateiname.txt"
```
Eingabe:
```Bash
Dies ist eine Zeile.
Und das ist eine andere.
```
Ausgabe:
```Bash
Dies ist eine Zeile.
Und das ist eine andere.
```
## Vertiefende Informationen:

Bash wurde 1989 erstellt und hat eine lange Geschichte des Umgangs mit Textdateien. Frühe Alternativen könnten `awk` oder `sed` sein, beide sind leistungsfähiger, aber komplizierter. Der Befehl `read` in Bash verwendet den internen File Descriptor, um den Inhalt der Datei zu lesen.

## Mehr dazu:

- [Bash Scripting Guide](https://tldp.org/LDP/abs/html/)
- [GNU Bash-Referenzhandbuch](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash-Programmierung von Wikibooks](https://en.wikibooks.org/wiki/Bash_Shell_Scripting) 

Erinnerung: Sicherheit ist ein Muss. Seien Sie vorsichtig mit den Skripten, die Sie ausführen, insbesondere wenn sie von unbekannten Quellen stammen.