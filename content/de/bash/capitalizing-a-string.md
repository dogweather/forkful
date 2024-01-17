---
title:                "String großschreiben"
html_title:           "Bash: String großschreiben"
simple_title:         "String großschreiben"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Was ist das und warum?

Capitalizing ist ein Konzept in der Programmierung, das es ermöglicht, bestimmte Zeichen in einem String in Großbuchstaben zu konvertieren. Programmierer nutzen dies, um Texte einheitlicher aussehen zu lassen oder um bestimmte Funktionen zu implementieren.

## Wie geht das?

Um einen String in Bash zu capitalizen, verwenden wir das Kommando ```echo```, gefolgt von dem String, den wir capitalizen möchten, gefolgt von dem Pipe-Operator ```|``` und dann dem Kommando ```tr```, gefolgt von den Optionen ```[:lower:]``` und ```[:upper:]```, gefolgt von Schrägstrichen ```/``` und abschließend dem Befehl ```sed```. Beispiel:

```Bash
echo "hallo welt" | tr '[:lower:]' '[:upper:]' | sed 's/\([A-Z]\)/\L\1/g'
```
Dieses Beispiel würde den String "hallo welt" in "HALLO WELT" umwandeln und ausgeben.

## Tiefer tauchen

Die Idee des Capitalizing kommt aus der Schreibmaschinentechnologie, bei der es keine Möglichkeit gab, zwischen Groß- und Kleinbuchstaben zu wechseln. Alternativen zum Capitalizing können die Verwendung von regulären Ausdrücken oder von speziellen Tools wie ```awk``` oder ```sed``` sein.

Es gibt auch verschiedene Implementierungen von Capitalizing, wie zum Beispiel die Funktion ```ucfirst```, die in der Programmiersprache PHP verfügbar ist und nur den ersten Buchstaben einer Zeichenkette in Großbuchstaben konvertiert.

## Siehe auch

- [Offizielle Bash Dokumentation](https://www.gnu.org/software/bash/manual/)
- [Ein Beitrag über das Capitalizing in Bash](https://www.lifewire.com/capitalize-words-string-bash-4618163)
- [Beispiele für die Verwendung von tr und sed](https://www.tecmint.com/13-tr-command-examples-in-linux/)