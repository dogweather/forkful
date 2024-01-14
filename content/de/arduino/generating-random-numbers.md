---
title:                "Arduino: Erzeugung von Zufallszahlen"
programming_language: "Arduino"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Randomisierte Zahlen sind ein wichtiges und nützliches Werkzeug in der Arduino Programmierung. Sie können verwendet werden, um zufällige Ereignisse zu simulieren, zufällige Entscheidungen zu treffen oder um Testdaten zu generieren. In diesem Blog-Beitrag werden wir uns genauer damit befassen, wie man in Arduino zufällige Zahlen erzeugen kann.

## Wie funktioniert es

Es ist sehr einfach, in Arduino zufällige Zahlen zu generieren. Dazu verwenden wir die Funktion `random(min, max)`, wobei `min` die kleinste und `max` die größte mögliche Zahl definiert. Hier ist ein Beispielcode:

```
Arduino.random(0, 10); // wird eine zufällige Zahl zwischen 0 und 10 ausgeben
```

Wir können auch eine zufällige Zahl auswählen, ohne eine obere Grenze anzugeben. Dazu verwenden wir die Funktion `random(max)`:

```
Arduino.random(100); // wird eine zufällige Zahl zwischen 0 und 100 ausgeben
```

Wenn wir zufällige Gleitkommazahlen haben möchten, können wir die Funktion `random(min, max)` zusammen mit der Funktion `map` verwenden, um die Zahl in einen Gleitkommawert zwischen 0 und 1 umzuwandeln:

```
map(Arduino.random(0, 1023), 0, 1023, 0.0, 1.0); // wird eine zufällige Gleitkommazahl zwischen 0.0 und 1.0 ausgeben
```

Um ein zufälliges Bit zu erzeugen, können wir die Funktion `random(2)` verwenden, die entweder 0 oder 1 zurückgibt.

Deep Dive

Die Funktion `random` verwendet einen pseudo-zufälligen Algorithmus, der jedes Mal, wenn das Programm ausgeführt wird, die gleiche Reihenfolge von Zahlen liefert. Wenn wir jedoch ein spezielles Startelement angeben, ändert sich die Reihenfolge der Zahlen. Dazu können wir die Funktion `randomSeed(number)` verwenden, wobei `number` ein Wert ist, der dazu verwendet wird, den Algorithmus zu starten.

Eine wichtige Sache zu beachten ist, dass wir in Arduino zwar zufällige Zahlen erzeugen können, aber diese sind nicht wirklich zufällig. Sie folgen einer bestimmten Sequenz, die durch den Algorithmus bestimmt wird. Wenn wir also wirklich zufällige Zahlen benötigen, sollten wir externe Geräte wie Sensoren verwenden.

## Siehe auch

Hier sind einige nützliche Ressourcen für weitere Informationen über das Generieren von zufälligen Zahlen in Arduino:

- [Arduino Random Library](https://www.arduino.cc/reference/en/language/functions/random-numbers/)
- [How to Generate Random Numbers in Arduino](https://arduinobasics.blogspot.com/2012/08/how-to-generate-random-numbers-in.html)
- [Pseudo-random number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)