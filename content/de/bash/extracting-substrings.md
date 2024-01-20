---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was und Warum?

Eine Teilzeichenkette (Substring) ist ein kleiner Teil einer gröszeren Zeichenkette (String). Substring-Extraktion in Bash ermöglicht das Isolieren bestimmter Bereiche des Strings. Dies ist nützlich zum Manipulieren von Dateipfaden, Parsing von Zeichenketten oder zur Suche nach bestimmten Mustern in Strings.

## Wie geht das:

Extrahieren von Substrings in Bash ist einfach, und es geht so:

```Bash
# Substring beginnend bei Index 0 mit Länge 5
string="Hallo, Welt!"
substring=${string:0:5}
echo $substring
```

Ausgabe:

```Bash
Hallo
```

Aber was, wenn wir vom Ende des Strings extrahieren wollen? Kein Problem:

```Bash
# Substring beginnend bei Index -5 (5 Zeichen vom Ende) mit Länge 5
string="Hallo, Welt!"
substring=${string:-5:5}
echo $substring
```

Ausgabe:

```Bash
Welt!
```

## Tiefer tauchen

Die Fähigkeit, Substrings in Bash zu extrahieren, ist seit der Bash-Version 2.0, veröffentlicht im Dezember 1996, verfügbar. Das Extrahieren von Substrings ist eine Standardfunktion in den meisten modernen Programmiersprachen, einschliesslich Python, Java und C++. 

Alternativen zur Substring-Extraktion in Bash können die Verwendung von `awk`, `sed` oder `cut` Linux/Unix-Befehlen sein. Diese können bei komplexeren Operationen nützlicher sein, erfordern jedoch zusätzliche Aufrufe zu externen Programmen, was die Effizienz beeinträchtigen kann.

Das Extrahieren von Substrings in Bash erfolgt über eine eingebaute Funktion, d.h. es werden keine externen Programme aufgerufen. Daher ist es in der Regel schneller als die Verwendung von `awk`, `sed`, oder `cut`.

## Siehe auch

Die offizielle GNU Bash-Dokumentation liefert weitere Details zur Zeichenkettensyntax und Substring-Extraktion:
[Gnu Bash Handbuch](https://www.gnu.org/software/bash/manual/bash.html#Shell-Parameter-Expansion)  

Für diejenigen, die mehr über das Finden und Extrahieren von Substrings lernen möchten, bietet StackOverflow interessante Diskussionen:
[StackOverflow Bash Substring](https://stackoverflow.com/questions/428109/extract-substring-in-bash)