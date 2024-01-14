---
title:                "Fish Shell: Zufallszahlen generieren"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Hast du schon einmal gezweifelt, ob die Zufallsfunktion in deinem Programm wirklich zufällige Zahlen generiert? Mit Fish Shell kannst du ganz einfach deine eigenen Zufallszahlen erstellen und die Kontrolle behalten.

## How To

Um deine eigenen Zufallszahlen zu generieren, musst du zunächst die Fish Shell integrieren. Das geht ganz einfach, indem du eine neue Fish Shell-Datei erstellst und diese in deinem Terminal öffnest.

```Fish Shell
$ touch zufallszahlen.fish
$ fish zufallszahlen.fish
```

Als nächstes definierst du die Anzahl der gewünschten Zufallszahlen und die Spannweite, aus der sie generiert werden sollen.

```Fish Shell
$ set anzahl 10
$ set spannweite 100
```

Jetzt kommt der spannende Teil - die Zufallszahlengenerierung. Dazu nutzt du den Befehl ```math``` mit der Funktion ```random``` und gibst die vorher definierte Spannweite an.

```Fish Shell
$ math (random $spannweite)
```

Diesen Befehl kannst du in einer for-Schleife nutzen, um die gewünschte Anzahl an Zufallszahlen zu erstellen.

```Fish Shell
$ for i in (seq $anzahl)
    math (random $spannweite)
end
```

Das Ergebnis sind 10 zufällige Zahlen zwischen 0 und 100.

```
16
38
74
55
84
9
90
41
56
26
```

## Deep Dive

Wenn du tiefer in die Thematik der Zufallszahlen generierung einsteigen möchtest, bietet die Fish Shell noch weitere Funktionen. So können zum Beispiel auch Zufallszahlen aus bestimmten Verteilungen erstellt werden, wie zum Beispiel einer Normalverteilung.

```Fish Shell
$ math (round (random-normal 100 10))
```

Dieser Befehl generiert eine Zufallszahl aus einer Normalverteilung mit einem Mittelwert von 100 und einer Standardabweichung von 10.

## Siehe auch

- [Fish Shell Dokumentation zur mathematischen Funktionen](https://fishshell.com/docs/current/cmds/math.html)
- [Weiterführende Informationen zu Zufallszahlen in der Programmierung](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)