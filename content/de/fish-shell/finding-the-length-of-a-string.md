---
title:                "Die Länge eines Strings finden"
html_title:           "Fish Shell: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge einer Zeichenkette zu finden bedeutet zu bestimmen, wie viele Zeichen in einer Zeichenkette vorhanden sind. Programmierer tun dies, um beispielsweise die Eingabe des Benutzers zu überprüfen oder um die Ausgabe zu formatieren.

## Wie geht's:
Um die Länge einer Zeichenkette in der Fish Shell zu finden, können wir das ```count```-Kommando verwenden. Hier ist ein Beispiel:

```
set str "Hallo Welt"
echo (count $str)
```

Dies wird ```11``` ausgeben, da die Zeichenkette "Hallo Welt" aus 11 Zeichen besteht.

## Tiefgehende Informationen:
Das Konzept, die Länge einer Zeichenkette zu bestimmen, ist nicht neu. Es wurde schon in früheren Programmiersprachen wie Fortran und COBOL verwendet. Ein alternatives Verfahren ist die Verwendung von Endmarkierungen, wie zum Beispiel das Hinzufügen eines Leerzeichens am Ende der Zeichenkette. In der Fish Shell wird die Funktion ```string length $str``` zur Ermittlung der Länge einer Zeichenkette verwendet.

## Siehe auch:
- [Fisch-Shell-Dokumentation](https://fishshell.com/docs/current/cmds/count.html)
- [Informationen zur String-Länge auf frei-programmieren.de](https://www.frei-programmieren.de/freie-programm-artikel/frei-programmieren-kleine-verwaltung/163-string-laenge)
- [Zusätzliche Informationen zur Geschichte der String-Länge](https://de.wikipedia.org/wiki/String)