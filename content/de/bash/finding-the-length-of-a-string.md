---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

---

## Was & Warum?

Das Finden der Länge eines Strings bedeutet, die Anzahl der Zeichen in einem Text zu zählen. Es hilft Programmierern, Fehler zu vermeiden, indem sie sicherstellen, dass der Text beispielsweise die erwartete Länge hat.

## So geht's:

Hier ist ein einfaches Beispiel, wie man die Länge eines Strings in Bash bestimmt:

```Bash
string="Hallo Welt"
echo ${#string}
```

Die Ausgabe wäre:

```Bash
11
```

## Tiefer Einblick

Historisch gesehen ist bash eine Befehlssprachen-Schnittstelle, die ursprünglich 1989 für das GNU-Projekt entwickelt wurde. Seitdem gibt es viele Methoden und Techniken zum Bestimmen der Länge eines Strings.

Eine weitere Methode, die Sie verwenden könnten, ist `expr $string : '.*'`. Diese Methode ist jedoch im Vergleich zu `${#string}` langsamer. 

Es ist wichtig zu beachten, dass wenn Sie versuchen, die Länge eines Strings zu finden, der spezielle Zeichen enthält, Sie möglicherweise auf Probleme stoßen könnten, wenn Sie diese Techniken verwenden. Hier kommt die Funktion `printf -v var %s "$string"` ins Spiel, die sicherstellt, dass alle Zeichen korrekt interpretiert werden.

## Siehe Auch

Für weitere Information:
- Stringhandhabung in Bash: https://tldp.org/LDP/abs/html/string-manipulation.html
- GNU Bash Dokumentation: https://www.gnu.org/software/bash/manual/bash.html
- StackOverflow Diskussion zur String Länge: https://stackoverflow.com/questions/17368067/length-of-string-in-bash