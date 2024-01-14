---
title:                "Bash: Verbinden von Zeichenketten"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Zeichenfolgen ist eine grundlegende Programmierfähigkeit, die in vielen Skripten und Programmen verwendet wird. Es ermöglicht es, mehrere Zeichenketten miteinander zu kombinieren, um komplexe Texte zu erstellen oder Variable in Ausgaben einzubinden.

## Wie man es macht

In Bash gibt es verschiedene Methoden, um Zeichenfolgen zu verketten. Eine häufig verwendete Methode ist die Verwendung des Befehls "echo" mit dem Pluszeichen (+), um zwei Zeichenfolgen miteinander zu verbinden.

```
Bash
echo "Hallo " + "Welt"
```

Dieser Befehl gibt "Hallo Welt" als Ergebnis aus. Man kann auch Variablen in die Zeichenfolgen einfügen und diese miteinander verketten.

```
Bash
name="Max"
echo "Hallo, mein Name ist $name"
```

Dieser Befehl gibt "Hallo, mein Name ist Max" als Ergebnis aus.

Es ist auch möglich, mehrere Variablen und Zeichenfolgen zu verketten, indem man diese in Klammern setzt.

```
Bash
name="Max"
greeting="Hallo"
echo "$greeting, mein Name ist $name"
```

Dieser Befehl gibt "Hallo, mein Name ist Max" aus.

## Tiefer Einblick

In Bash sind Zeichenfolgen immer in einfachen Anführungszeichen oder in doppelten Anführungszeichen zu schreiben. Dies ermöglicht es, spezielle Zeichen wie Leerzeichen oder Sonderzeichen zu verwenden, ohne dass diese als Teil des Befehls interpretiert werden.

Außerdem ist zu beachten, dass in Bash die Verkettung von Zeichenfolgen durch ein Leerzeichen erfolgt. Wenn also ein Leerzeichen zwischen den Zeichenfolgen steht, werden diese nicht miteinander verketten, sondern einfach hintereinander ausgegeben.

## Siehe auch

- [Offizielle Bash Dokumentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutorial: String operations in Bash](https://linuxize.com/post/bash-concatenate-strings/)
- [Bash String Concatenation Cheat Sheet](https://devhints.io/bash#string-concatenation)