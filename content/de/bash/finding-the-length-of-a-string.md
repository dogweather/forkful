---
title:                "Ermitteln der Länge eines Strings"
html_title:           "Bash: Ermitteln der Länge eines Strings"
simple_title:         "Ermitteln der Länge eines Strings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Möchtest du wissen, wie lang ein bestimmter Text in deinem Bash-Skript ist? Das Finden der Länge einer Zeichenkette kann sehr nützlich sein, zum Beispiel bei der Validierung von Eingaben oder der Manipulation von Daten.

## Wie geht's

Um die Länge einer Zeichenkette in Bash zu finden, gibt es mehrere Möglichkeiten. Eine davon ist die Verwendung von `expr length`, wie im folgenden Beispiel:

```Bash
string='Hallo Welt'
echo "Die Länge von $string ist $(expr length $string)"
```

Die Ausgabe für dieses Skript wäre "Die Länge von Hallo Welt ist 11".

Eine andere Möglichkeit ist die Verwendung von `wc -c`, um die Anzahl der Zeichen zu zählen (diese Methode zählt jedoch alle Zeichen, einschließlich Leerzeichen und Sonderzeichen):

```Bash
string='Hallo Welt'
echo "Die Länge von $string ist $(echo -n $string | wc -c)"
```

Die Ausgabe für dieses Skript wäre ebenfalls "Die Länge von Hallo Welt ist 11".

## Deep Dive

Der Befehl `expr length` verwendet den `POSIX`-Standard `expr` und gibt die Länge der Zeichenkette in Zeichen zurück. Wenn du jedoch auch mehrsprachige Zeichen oder Sonderzeichen berücksichtigen möchtest, solltest du `wc -m` verwenden, welches die Anzahl der Bytes zurückgibt. Weitere Informationen findest du in der Dokumentation von `wc`.

## Siehe auch

- Bash-Befehl: `expr`
- Bash-Befehl: `wc`
- [Offizielle Bash-Dokumentation](https://www.gnu.org/software/bash/manual/bash.html)