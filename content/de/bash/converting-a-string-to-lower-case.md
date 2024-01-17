---
title:                "Umwandeln einer Zeichenkette in Kleinbuchstaben"
html_title:           "Bash: Umwandeln einer Zeichenkette in Kleinbuchstaben"
simple_title:         "Umwandeln einer Zeichenkette in Kleinbuchstaben"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Umwandlung eines Strings in Kleinbuchstaben ist ein häufig verwendetes Verfahren in der Programmierung. Dadurch wird sichergestellt, dass eine bestimmte Zeichenfolge immer in einem einheitlichen Format vorliegt und es somit einfacher wird, diese zu vergleichen oder zu verarbeiten.

## So geht's:
Eine Zeichenfolge in Bash in Kleinbuchstaben zu konvertieren, ist relativ einfach. Dazu kann die integrierte Funktion "tr" verwendet werden, die Zeichen ersetzt und somit eine Umwandlung ermöglicht. Der Befehl sieht dabei wie folgt aus:

``` Bash
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'
```

Die Ausgabe dieses Befehls wäre dann "hello world", da alle Großbuchstaben in Kleinbuchstaben umgewandelt wurden. Es ist auch möglich, direkt in einem Skript eine Zeichenfolge in kleinere Buchstaben umzuwandeln, indem man den Befehl in Variablen speichert und diese dann verwendet.

## Tiefere Einblicke:
In der Geschichte der Programmierung gab es verschiedene Ansätze, um Strings in Kleinbuchstaben zu konvertieren. Früher waren oft komplizierte Algorithmen nötig, um diese Umwandlung durchzuführen. Heutzutage gibt es jedoch effiziente und integrierte Funktionen, die diesen Prozess vereinfachen.

Eine alternative Methode, um Zeichenfolgen in Kleinbuchstaben zu konvertieren, ist die Verwendung von Programmiersprachen wie Python oder Java, die eigene Funktionen für diesen Zweck bereitstellen. Diese können je nach Anwendungsfall möglicherweise auch schneller sein als der Einsatz von Bash.

Die Implementierung von Kleinbuchstabenkonvertierung in Bash basiert auf der Nutzung von regulären Ausdrücken und dem Austausch von Zeichen im String. Daher kann es in manchen Fällen auch zu unerwartetem Verhalten kommen, wenn beispielsweise Sonderzeichen oder Umlaute verwendet werden.

## Weitere Informationen:
- [Bash tr command](https://www.geeksforgeeks.org/tr-command-in-linux-with-examples/)
- [Python lower() function](https://www.w3schools.com/python/ref_string_lower.asp)
- [Java toLowerCase() method](https://www.w3schools.com/java/ref_string_tolowercase.asp)