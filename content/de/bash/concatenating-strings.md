---
title:                "Verkettung von Zeichenketten"
html_title:           "Bash: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Konkatenieren von Zeichenfolgen ist einfach ein fancy Way, um zwei oder mehr Strings zusammenzufügen. Programmierer tun dies oft, um längere Texte oder Variablen zusammenzubauen.

## Wie geht's?

Hier sind ein paar Beispiele, wie man mit Bash in der aktuellen Version Strings konkatenieren kann:

```
# Beispiel mit einfachen Strings
str1="Hallo"
str2="Welt"
echo "$str1 $str2" # Ausgabe: Hallo Welt

# Beispiel mit Variablen
vorname="John"
nachname="Doe"
echo "Mein Name ist $vorname $nachname" # Ausgabe: Mein Name ist John Doe

# Beispiel mit kombinierten Strings
echo "Linux" + "Bash" # Ausgabe: Linux Bash
```

## Tieferes Eintauchen

Das Konkatenieren von Zeichenfolgen ist keine neuartige Idee, es wird seit langem in verschiedenen Programmiersprachen verwendet. Einige Alternative Methoden, die strings in Bash zusammenzufügen wären: ```str1="$str1$str2"``` oder ```str1+=$str2```. Es gibt auch Implementierungsdetails zu beachten, wie z.B. die Verwendung von Escape-Sequenzen, um Sonderzeichen innerhalb der Strings zu behandeln.

## Siehe auch

- [Bash-Dokumentation] (https://www.gnu.org/software/bash/manual/)
- [GNU Projektseite für Bash] (https://www.gnu.org/software/bash/)