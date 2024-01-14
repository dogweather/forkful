---
title:                "Arduino: Zufällige Zahlen generieren."
simple_title:         "Zufällige Zahlen generieren."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Die Generierung von Zufallszahlen ist ein wichtiger Aspekt der Arduino-Programmierung. Durch das Erzeugen von Zufallszahlen können wir unberechenbare und vielfältige Verhaltensmuster erzeugen, die unsere Projekte interessanter und interaktiver machen.

## Wie es geht

Um Zufallszahlen mit Arduino zu generieren, verwenden wir die Funktion `random(min,max)`, die eine zufällige Ganzzahl zwischen dem angegebenen minimalen und maximalen Wert zurückgibt. Hier ist ein Beispielcode, der eine zufällige Zahl zwischen 1 und 10 generiert und sie auf dem Seriellen Monitor ausgibt:

```
Arduino

// Definiere zwei Variablen als Minimalwert und Maximalwert
int min = 1;
int max = 10;

void setup() {
  // Beginne die Kommunikation mit dem Seriellen Monitor
  Serial.begin(9600);

  // Initialisiere den Zufallsgenerator
  randomSeed(analogRead(0));
}

void loop() {
  // Rufe die Funktion random(min,max) auf und speichere die zufällige Zahl in der Variable "zufall"
  int zufall = random(min, max+1);

  // Gib die zufällige Zahl auf dem Seriellen Monitor aus
  Serial.println(zufall);

  // Warte 1 Sekunde, bevor der Code wieder von vorne ausgeführt wird
  delay(1000);
}

```

Die Ausgabe auf dem Seriellen Monitor könnte beispielsweise folgendermaßen aussehen:

```
3
8
5
1
10
```

## Tiefeneintauchung

Die Zufallsfunktion von Arduino verwendet den physikalischen Rausch des Analog-Digital-Wandlers, um eine zufällige Startposition für den Zufallsgenerator zu erhalten. Die Funktion `random()` verwendet dann einen mathematischen Algorithmus, um aus dieser Startposition eine "zufällige" Folge von Zahlen zu erzeugen. Diese Zahlen sind natürlich nicht wirklich zufällig, aber für die meisten Anwendungsfälle sind sie ausreichend und bieten ein hohes Maß an Unvorhersehbarkeit.

Es ist jedoch wichtig zu beachten, dass der Zufallsgenerator standardmäßig immer die gleiche Sequenz von Zahlen ausgibt, wenn der Arduino neu gestartet wird. Um dies zu vermeiden, verwenden wir die Funktion `randomSeed(analogRead(0))`, die den Zufallsgenerator jedes Mal mit einem anderen Startwert versorgt.

Wir können auch den Zufallsgenerator durch die Funktion `randomSeed(seed)` mit einem bestimmten Startwert initialisieren, um spezifische Sequenzen von Zufallszahlen zu erhalten. Dies kann nützlich sein, wenn wir bestimmte Muster oder Verhaltensweisen erzeugen möchten.

Eine weitere Möglichkeit, die Zufallszahlen zu beeinflussen, besteht darin, die Werte der Variablen `min` und `max` zu ändern, die in der Funktion `random()` verwendet werden. Indem wir diese Werte anpassen, können wir den Bereich der generierten Zufallszahlen verändern.

## Siehe auch

- [Arduino Referenzdokumentation für die Funktion random()](https://www.arduino.cc/reference/de/language/functions/random-numbers/random/)
- [Ein Tutorial zur Generierung von Zufallszahlen mit Arduino](https://lastminuteengineers.com/random-numbers-digital-arduino/)