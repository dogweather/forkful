---
title:                "Erzeugung von Zufallszahlen"
html_title:           "Arduino: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Generieren von zufälligen Zahlen ist ein wichtiger Teil des Programmierens, da es ermöglicht, unvorhersehbare Ergebnisse zu erzeugen. Das kann nützlich sein, um beispielsweise Spiele zu entwickeln oder Sicherheitscodes zu erstellen.

## Wie geht's?
Das Arduino-Board verfügt über eine integrierte Funktion namens ```random()```, die zufällige Zahlen zwischen 0 und 1 zurückgibt. Sie kann auf verschiedene Arten verwendet werden, um Zahlen mit einem bestimmten Bereich oder Format zu generieren. Hier ist ein Beispiel, um eine zufällige Ganzzahl zwischen 1 und 10 zu erhalten:

```Arduino 
int x = random(1, 11); 
```

Die Ausgabe könnte zum Beispiel ```5``` sein.

## Tiefer Tauchen
Zufallszahlen spielen in vielen Bereichen der Informatik eine wichtige Rolle. Sie werden nicht nur in Spielen, sondern auch in Kryptographie und statistischen Analysen verwendet. Es gibt auch verschiedene Algorithmen und Methoden, um zufällige Zahlen zu generieren, wie zum Beispiel der Middle-Square-Algorithmus oder der Mersenne-Twister.

## Siehe auch
- [Arduino Referenz zu random()](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Erklärvideo zu Zufallszahlen in der Informatik (auf Deutsch)](https://www.youtube.com/watch?v=TdPL-cAOq8M)
- [Wikipedia-Artikel über Zufallszahlen](https://de.wikipedia.org/wiki/Zufallszahl)