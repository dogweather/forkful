---
title:    "Fish Shell: Zufallszahlen generieren"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum Generieren von Zufallszahlen Sinn macht

Das Generieren von Zufallszahlen ist ein häufiger Bestandteil von Programmieraufgaben. Es kann verwendet werden, um zufällige Auswahl oder Variationen in Spielen, statistischen Simulationen oder Verschlüsselungsalgorithmen zu erzeugen.

## Wie man Zufallszahlen in Fish Shell generiert

```Fish Shell
# Verwendung des 'shuf' Befehls, um eine Liste von zufälligen Zahlen zu generieren
shuf -i 1-100
# Ausgabe: 86 7 54 91 33 29 18 6 36 83 58 68 78 30 53 74 99 12 70 92 55 61 25 45 47 13 85 23 96 95 37 35 76 20 88 44 66 16 52 38 67 50 42 46 79 14 
```

```Fish Shell
# Verwendung von '/dev/random' oder '/dev/urandom' Dateien, um zufällige Zahlen zu generieren
head -10 /dev/random
# Ausgabe: 150 205 49 34 245 221 33 16 173 249
```

## Tiefere Einblicke in die Generierung von Zufallszahlen

Es gibt verschiedene Methoden zur Generierung von Zufallszahlen. Einige basieren auf statistischen Algorithmen, während andere auf physischen Ereignissen, wie der Quantisierung von Rauschen, basieren. Das Betriebssystem verwendet oft auch verschiedene Methoden, um Zufallszahlen zu generieren, die von der Hardware und der Umgebung abhängig sind.

## Siehe auch

- [Fish Shell Dokumentation zu shuf Befehl](https://fishshell.com/docs/current/cmds/shuf.html)
- [Artikel über die Verwendung von /dev/random und /dev/urandom Dateien](https://www.gnu.org/software/coreutils/manual/html_node/dev_002frandom.html)
- [Wikipedia Artikel über Pseudorandom Generatoren](https://de.wikipedia.org/wiki/Pseudorandom_number_generator)