---
title:    "Gleam: Zufallszahlen generieren"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiger Bestandteil vieler Programmiervorgänge. Ob bei der Entwicklung von Spielen, der Durchführung von Statistiken oder der Verschlüsselung von Daten, Zufallszahlen sind unerlässlich. In diesem Blogbeitrag werden wir uns ansehen, wie man in Gleam Zufallszahlen generiert und wie man sie in verschiedenen Anwendungsfällen nutzen kann.

## How To

Um Zufallszahlen in Gleam zu generieren, können wir die Funktion `random.float` verwenden. Diese Funktion gibt eine zufällige Fließkommazahl zwischen 0 und 1 zurück. Um eine ganze Zufallszahl zu erhalten, können wir `random.int` verwenden, wobei wir den Wertebereich als Parameter angeben müssen.

```Gleam
import random

random_float = random.float()

random_int = random.int(1, 10)
```

Die Ausgabe in diesem Beispiel könnte zum Beispiel `0.456789` für `random_float` und `4` für `random_int` sein. Um die Zufallszahl jedes Mal unterschiedlich zu halten, können wir auch einen Seed-Wert angeben, der als zweiter Parameter an die Funktion übergeben wird.

## Deep Dive

Die Generierung von Zufallszahlen in Gleam basiert auf dem Mersenne Twister Algorithmus. Dabei handelt es sich um einen Pseudozufallszahlengenerator, der auf einer Reihe von mathematischen Berechnungen basiert. Dieser Algorithmus wurde ausgewählt, da er eine ausreichend hohe Periodenlänge hat und als sicher für die meisten Anwendungen gilt.

Es ist jedoch wichtig zu beachten, dass diese Zufallszahlen nicht wirklich zufällig sind, sondern lediglich auf der Grundlage von Berechnungen erzeugt werden. Für kryptografische Anwendungen sollten daher andere Methoden zur Generierung von Zufallszahlen verwendet werden.

## Siehe auch

Für weitere Informationen und Anwendungsmöglichkeiten zu Zufallszahlen in Gleam, siehe die folgenden Links:

- Offizielle Gleam Dokumentation: https://gleam.run/documentation/index.html#internals.random
- Eine Einführung in Zufallszahlen in der Programmierung: https://www.freecodecamp.org/news/how-to-generate-random-numbers-in-programming-418aec8f6e3a/
- Das Mersenne Twister Verfahren im Detail: https://www.makeuseof.com/tag/mersenne-twister-generate-random-youtube-videos/