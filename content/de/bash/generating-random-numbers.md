---
title:    "Bash: Erzeugung von Zufallszahlen"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiges Konzept in der Programmierung, um zufällige Variationen in Code oder Anwendungen zu ermöglichen. Es kann nützlich sein, um Spiele zu erstellen, zufällige Testdaten zu generieren oder sogar Sicherheitsmaßnahmen zu implementieren.

## Wie geht man vor

Es gibt verschiedene Möglichkeiten, um in Bash Zufallszahlen zu generieren. Eine Möglichkeit ist die Verwendung des Befehls `shuf`. Dieser Befehl kann verwendet werden, um eine zufällige Anordnung von Zeilen in einer Datei zu erzeugen. Zum Beispiel:

```Bash
shuf datei.txt
```

Dies wird eine zufällige Anordnung der Zeilen in der Datei `datei.txt` ausgeben. Eine weitere Möglichkeit ist die Verwendung des Befehls `od`, der den Inhalt einer Datei in eine binäre Darstellung konvertiert. Zum Beispiel:

```Bash
od -An -N2 -t u2 /dev/urandom
```

Dies wird zwei zufällige Zahlen ausgeben, die mit einer Ganzzahl repräsentiert werden.

## Tiefer Einblick

Eine Möglichkeit, tiefere Kontrolle über die generierten Zufallszahlen zu haben, ist die Verwendung der integrierten Bash-Funktion `$RANDOM`. Diese Funktion gibt eine zufällige Ganzzahl zwischen 0 und 32767 zurück. Sie kann in einer Schleife verwendet werden, um eine bestimmte Anzahl von Zufallszahlen zu generieren. Zum Beispiel:

```Bash
for i in {1..10}
do
    echo $RANDOM
done
```

Dies wird 10 zufällige Zahlen ausgeben, jedes Mal wenn die Schleife durchläuft. Eine weitere Option ist die Verwendung der Bash-Bibliothek `random`, die zusätzliche Funktionen zum Generieren von Zufallszahlen bietet.

## Siehe auch

- [Bash-Dokumentation: shuf](https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html)
- [Bash-Dokumentation: $RANDOM](https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html)
- [Bash-Bibliothek: random](https://www.gnu.org/software/bash/manual/html_node/Bash-Variables.html#index-RANDOM)