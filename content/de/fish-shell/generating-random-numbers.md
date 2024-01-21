---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:01.208350-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Generieren von Zufallszahlen bedeutet, eine Zahl aus einem bestimmten Bereich zu ziehen, die nicht vorhersagbar ist. Programmierer nutzen Zufallszahlen für alles Mögliche, von der Datenanalyse bis hin zu Spielen.

## So geht's:
Verwenden von `random` in Fish, um eine Zufallszahl zu erhalten:

```Fish Shell
set -l zufallszahl (random)
echo $zufallszahl
```

Und so sieht's aus:

```
42145
```

Um eine Zufallszahl zwischen 1 und 100 zu generieren:

```Fish Shell
set -l zufallszahl (random 1 100)
echo $zufallszahl
```

Beispielhafte Ausgabe:

```
57
```

## Deep Dive
Früher nutzten viele die `$RANDOM` Variable, die in anderen Shells wie Bash verfügbar ist. Doch Fish macht es einfacher mit dem Befehl `random`, der direkt eingebaut ist. `random` stellt sicher, dass man gleichmäßig verteilte Werte bekommt. Es basiert auf Pseudozufallszahlen-Generatoren (PRNGs), die einen Seed verwenden, um Sequenzen zu erzeugen, die zufällig erscheinen.

## Siehe auch
- Fish Shell Dokumentation zu `random`: https://fishshell.com/docs/current/cmds/random.html
- Wikipedia-Artikel über Pseudozufallszahlen-Generatoren: https://de.wikipedia.org/wiki/Pseudozufallszahlengenerator