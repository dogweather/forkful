---
title:    "Arduino: Zufällige Zahlen erzeugen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Erzeugen von Zufallszahlen ist hilfreich in verschiedenen Anwendungsbereichen, wie beispielsweise Kryptographie, Simulations- oder Spielprogrammierung. Durch die Verwendung von Zufallszahlen können Programme mehr Abwechslung und Unvorhersehbarkeit erreichen.

## So geht's

Die Verwendung der Random-Funktion in Arduino ermöglicht es uns, schnell und einfach Zufallszahlen zu erstellen. Hier ist ein Beispiel, wie du eine Zufallszahl zwischen 0 und 100 generieren kannst:

```Arduino
int randomNum = random(0, 101);
Serial.println(randomNum);
```

Dieses Stück Code wird eine Zufallszahl zwischen 0 und 100 erzeugen und sie über die Serielle Schnittstelle ausgeben.

## Tiefergehende Informationen

Es gibt verschiedene Techniken zur Generierung von Zufallszahlen, darunter die Verwendung von physikalischen Prozessen oder Algorithmen. Arduino verwendet einen Algorithmus namens Linear Congruential Generator (LCG), um Zufallszahlen zu erzeugen. Dieser Algorithmus basiert auf einer einfachen, wiederholenden mathematischen Operation und liefert relativ zufällige Zahlen. Wenn du mehr über die Details dieses Algorithmus erfahren möchtest, findest du hier einige interessante Ressourcen:

- [Einführung zum LCG-Algorithmus](https://www.wolframalpha.com/examples/mathematics/number-theory/linear-congruential-generator/)
- [LCG in der Arduino-Dokumentation](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)

## Siehe auch

- [Weitere Informationen über die Random-Funktion in Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Interaktives Beispiel für die Verwendung von Zufallszahlen in Arduino](https://www.tinkercad.com/things/0vpsiLxztPV-random-numbers)