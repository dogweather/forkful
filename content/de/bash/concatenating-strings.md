---
title:                "Bash: Verkettung von Zeichenketten"
simple_title:         "Verkettung von Zeichenketten"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

# Warum

Das Verketten von Strings ist eine wichtige Fähigkeit in der Bash-Programmierung. Es ermöglicht es uns, verschiedene Textfragmente miteinander zu verbinden und somit dynamischere und effizientere Skripte zu erstellen.

## Wie

Um Strings in Bash miteinander zu verknüpfen, können wir die spezielle Variable ```$``` verwenden. Wir deklarieren zuerst die beiden Strings, die wir verketten möchten, und verwenden dann die Variable in einem ```echo```-Statement, um die Strings zusammenzufügen:

```
first_string="Hallo"
second_string="Welt"

echo "$first_string $second_string"
```

Die Ausgabe dieses Skripts würde folgendermaßen lauten: ```Hallo Welt```

Wir können auch mehrere Strings auf einmal verketten, indem wir sie alle in der gleichen ```echo```-Anweisung platzieren und sie durch Leerzeichen trennen:

```
first_name="Max"
last_name="Mustermann"

echo "Mein Name ist $first_name $last_name"
```

Die Ausgabe dieses Skripts würde lauten: ```Mein Name ist Max Mustermann```

## Tiefergehende Informationen

Beim Verketten von Strings in Bash gibt es einige Dinge zu beachten. Zum Beispiel können wir nicht einfach Leerzeichen zu Strings hinzufügen, indem wir diese direkt in den String schreiben. Stattdessen müssen wir das Leerzeichen innerhalb der ```echo```-Anweisung platzieren:

```
name="Hans"

echo "Hallo $name, wie geht es dir?"
```

Die Ausgabe wäre in diesem Fall: ```Hallo Hans, wie geht es dir?```

Ein weiteres nützliches Feature beim Verketten von Strings ist die Verwendung von Escape-Sequenzen, um spezielle Zeichen zu erzeugen. Zum Beispiel können wir mit der Sequenz ```\n``` einen Zeilenumbruch innerhalb eines Strings erzeugen:

```
sentence="Dies ist der erste Satz.\nDies ist der zweite Satz."

echo -e "$sentence"
```

Die Ausgabe wäre in diesem Fall:
```
Dies ist der erste Satz.
Dies ist der zweite Satz.
```

## Siehe auch

- [Bash Guide for Beginners (Deutsch)](http://bashem.de/Bash-Beginners-Guide/Bash-Beginners-Guide.html)
- [Bash Scripting Tutorial (Deutsch)](https://wiki.ubuntuusers.de/Bash-Scripting-Guide_/_Inhaltsverzeichnis/)
- [Offizielle Bash-Dokumentation (Deutsch)](https://ss64.com/bash/)

Danke fürs Lesen und viel Spaß beim Programmieren in Bash!