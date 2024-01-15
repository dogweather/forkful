---
title:                "Generierung von Zufallszahlen"
html_title:           "Bash: Generierung von Zufallszahlen"
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Bash"
category:             "Bash"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon immer mal wissen wolltest, wie man zufällige Zahlen generiert oder ob du in einem Spiel wirklich zufälligere Ergebnisse bekommen kannst, bist du hier richtig! In diesem Artikel zeigen wir dir, wie du mit Bash ganz einfach Zufallszahlen erzeugen kannst.

## Wie geht es?

Um zufällige Zahlen zu generieren, verwendet man in der Regel den Befehl "shuf" in Verbindung mit der Option "-i". Mit dieser Option können wir einen Bereich von Zahlen angeben, aus dem dann zufällig Zahlen ausgewählt werden. Ein Beispiel würde so aussehen:

```Bash
shuf -i 1-10
```

Dieser Befehl würde uns 10 zufällige Zahlen zwischen 1 und 10 ausgeben. Wenn du diese Zahlen in einer Datei speichern möchtest, kannst du den Befehl so erweitern:

```Bash
shuf -i 1-10 > zufallszahlen.txt
```

In der Datei "zufallszahlen.txt" würdest du dann die 10 zufälligen Zahlen finden. Du kannst auch die Anzahl der Zahlen, die du haben möchtest, angeben, indem du die Option "-n" verwendest. Zum Beispiel:

```Bash
shuf -i 1-100 -n 5
```

Dieser Befehl würde 5 zufällige Zahlen zwischen 1 und 100 ausgeben. Wenn du möchtest, dass die Zahlen durch Tabulatoren getrennt werden, kannst du die Option "-e" verwenden. Zum Beispiel:

```Bash
shuf -i 1-100 -n 10 -e
```

Dieser Befehl würde uns 10 zufällige Zahlen zwischen 1 und 100 ausgeben, getrennt durch Tabulatoren.

## Tiefer Einblick

Natürlich gibt es noch viele weitere Optionen, die du bei der Verwendung von "shuf" ausprobieren kannst. Zum Beispiel kannst du auch Buchstaben oder andere Zeichen anstelle von Zahlen verwenden, indem du die Option "-r" verwendest. Diese Option sorgt dafür, dass die Zahlen ohne Wiederholung ausgegeben werden.

```Bash
shuf -r -e a b c d e f
```

Dieser Befehl würde uns 6 zufällige Buchstaben ausgeben, ohne dass ein Buchstabe wiederholt wird. Neben "shuf" gibt es auch noch andere Möglichkeiten, um zufällige Zahlen zu generieren, wie zum Beispiel "jot" und "od". Diese kannst du gerne auch ausprobieren und schauen, welches Tool am besten zu deinen Bedürfnissen passt.

## Siehe auch

- Offizielle Dokumentation des Befehls "shuf": <https://www.gnu.org/software/coreutils/manual/html_node/shuf-invocation.html>
- Weitere Tools zur Generierung von Zufallszahlen unter Bash: <https://www.tecmint.com/generate-random-numbers-usage-examples/>