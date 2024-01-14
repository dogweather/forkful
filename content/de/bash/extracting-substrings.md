---
title:                "Bash: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Substrings oder Teilstrings (auch bekannt als Unterstrings) sind Teil einer Zeichenfolge oder eines Textes, der aus einer bestimmten Anzahl von Zeichen besteht. Das Extrahieren von Substrings kann nützlich sein, um bestimmte Informationen aus einem Text zu erhalten, beispielsweise um die Postleitzahl aus einer Adresse zu extrahieren. In diesem Blog-Beitrag werden wir uns ansehen, wie man Substrings in Bash extrahiert und warum dies eine nützliche Fähigkeit für jeden Programmierer sein kann.

## Wie geht man vor

Um Substrings in Bash zu extrahieren, können wir uns der integrierten Funktion `cut` bedienen. Diese Funktion ermöglicht es uns, eine beliebige Anzahl von Zeichen von einer bestimmten Position in der Zeichenfolge zu extrahieren. Zum Beispiel, wenn wir den Teilstring mit der Postleitzahl aus einer Adresse extrahieren wollen, können wir folgende Zeile verwenden:

```Bash
echo "123 Main St., Anytown, USA 12345" | cut -c 24-28
```

In diesem Beispiel extrahieren wir die Zeichen 24 bis 28 von der Eingabezeichenfolge, was der Postleitzahl entspricht. Die Ausgabe würde dann "12345" sein.

Wir können auch die `cut` Funktion verwenden, um Substrings basierend auf einem Trennzeichen zu extrahieren. Zum Beispiel, wenn wir eine Liste von Namen haben und nur die Nachnamen extrahieren möchten, können wir folgenden Bash-Code verwenden:

```Bash
names="John Smith, Jane Doe, Bob Jones"
echo $names | cut -d " " -f 2
```

Die Option `-d` gibt das Trennzeichen an, in diesem Fall ein Leerzeichen, und die Option `-f` gibt die Feldnummer an, die extrahiert werden soll. In diesem Beispiel wird der zweite Teil des Namens, also der Nachname, extrahiert.

Man kann auch mit der `awk` Funktion Substrings in Bash extrahieren. Diese ermöglicht es uns, basierend auf einem regulären Ausdruck zu suchen und zu extrahieren. Hier ein Beispiel, um alle Zahlen aus einem Text zu extrahieren:

```Bash
echo "Hello 123, bye 456" | awk '{print gensub(/([0-9]+)/,"\\1","g")}'
```

Die `awk` Funktion sucht nach jeder Zahl, die durch den regulären Ausdruck `([0-9]+)` dargestellt wird, und extrahiert sie in eine separate Zeile.

## Tiefgehende Einblicke

Das Extrahieren von Substrings ist nicht nur nützlich, um bestimmte Informationen aus einem Text zu erhalten, sondern es hilft auch beim Datenhandling und Parsing von Dateien. Zum Beispiel können CSV-Dateien mit Hilfe von Substrings leicht in separate Spalten aufgeteilt werden. Außerdem ist das Extrahieren von Substrings ein grundlegender Bestandteil des String-Manipulationsprozesses und kann in vielen anderen Programmiersprachen angewendet werden.

## Siehe auch

- [Beginner's Guide to Bash Scripting](https://www.pluralsight.com/blog/it-ops/linux-scripts-beginners-guide)
- [How to Use Awk for Data Manipulation](https://www.section.io/engineering-education/how-to-use-awk-for-data-manipulation/)
- [Mastering Bash: Substrings](https://linuxhint.com/bash_substring/)