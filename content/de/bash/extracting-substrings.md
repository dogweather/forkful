---
title:    "Bash: Extrahieren von Teilstrings"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilzeichenketten ist eine nützliche Fähigkeit in Bash, die es Ihnen ermöglicht, bestimmte Teile eines Strings zu isolieren und zu manipulieren. Dies kann hilfreich sein, wenn Sie beispielsweise bestimmte Informationen aus einer längeren Zeichenkette extrahieren oder eine Zeichenkette in kleinere Teile zerlegen müssen.

## Wie man es macht

Um eine Teilzeichenkette in Bash zu extrahieren, verwenden Sie einfach die Syntax `${string:position:length}`, wobei "string" der ursprüngliche String ist, "position" der Startindex der zu extrahierenden Teilzeichenkette und "length" die Länge der zu extrahierenden Teilzeichenkette ist.

Ein Beispiel:

```Bash
sentence="Ich gehe spazieren."
echo ${sentence:8:8}
```

Dies gibt "spazieren" als Ausgabe aus, da wir bei Index 8 beginnen und 8 Zeichen extrahieren.

Wenn Sie nur den Teil am Anfang einer Zeichenkette extrahieren möchten, können Sie einfach die Länge weglassen:

```Bash
name="Max Mustermann"
echo ${name:0:3}
```

Dies gibt "Max" als Ausgabe aus.

Sie können auch negative Indizes verwenden, um die Teilzeichenkette von rechts nach links zu extrahieren. Zum Beispiel:

```Bash
phrase="Ich liebe Bash!"
echo ${phrase:6:-1}
```

Dies gibt "Bash" als Ausgabe aus, da wir bei Index 6 beginnen und bis zum Ende der Zeichenkette gehen.

## Tiefergehende Informationen

In Bash können Sie auch das sogenannte "Substring Expansion" verwenden, um Teilzeichenketten zu extrahieren. Dies beinhaltet die Verwendung von `%` und `#` in Variablenzuweisungen.

Ein Beispiel:

```Bash
surname="Max Mustermann"
echo ${surname% *}
```

Dies gibt "Max" als Ausgabe aus, da `% *` besagt, dass das längste vorkommende Leerzeichen und alle Zeichen danach entfernt werden sollen.

Weitere Informationen und Beispiele zum Thema Extrahieren von Teilzeichenketten in Bash finden Sie in der offiziellen Dokumentation.

## Siehe auch

- [Bash Manual: Substring Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html#Shell-Parameter-Expansion)
- [Explain Shell: Substring Expansion](https://explainshell.com/explain/1/bash/?x=-jw#%24%7Bvar%3A%25%7Bpattern%7D%7D)