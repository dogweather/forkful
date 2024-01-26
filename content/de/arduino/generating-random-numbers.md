---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:44.583705-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Erzeugen zufälliger Zahlen ist die Erstellung von Nummern, die keine erkennbare Ordnung oder Muster aufweisen. Programmierer verwenden Zufallszahlen für Spiele, Simulationen und um das Verhalten von Anwendungen zu variieren, damit Tests weniger vorhersehbar und realistischer werden.

## Wie geht das?
Hier ist ein einfaches Beispiel, das zeigt, wie man in Arduino zufällige Zahlen generiert:

```arduino
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0));
}

void loop() {
  int zufallszahl = random(1, 100); // Erzeugt eine Zufallszahl zwischen 1 und 99.
  Serial.println(zufallszahl);
  delay(1000); // Warte eine Sekunde zwischen den Zahlen.
}
```
Ausgabe im Seriellen Monitor:
```
23
59
88
...
```

## Tiefgang
Zufallszahlen in Mikrocontroller-Programmen haben eine spannende Geschichte. Frühe Computer nutzten externes Rauschen oder andere physikalische Prozesse für echte Zufälligkeit. Der `random()` Befehl in Arduino simuliert Zufälligkeit, verwendet aber einen Algorithmus – es sind Pseudozufallszahlen. Das `randomSeed()` legt den Startwert ("seed") für die Zufallsgenerierung fest – analogRead(0) nimmt ein wenig "Rauschen" vom unbenutzten Analogpin, um den Seed weniger vorhersehbar zu machen. Alternativen umfassen das Nutzen von externen Zufallszahlen-Generatoren. 

In der Implementierung solltest du beachten, dass ohne Änderung des Seeds bei jedem Neustart vom Arduino die gleiche Zahlenfolge erzeugt wird. Verschiedene Seeds erzeugen unterschiedliche Folgen, was die Vielfalt und Unvorhersagbarkeit verbessert. 

Für Anwendungen, die eine höhere Qualität an Zufälligkeit benötigen, wie etwa Kryptografie, reicht diese Methode nicht aus, und man sollte auf verschlüsselte Zufallszahlen-Generatoren zurückgreifen.

## Siehe auch
- Arduino Referenz für `random()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Arduino Referenz für `randomSeed()`: https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/
- Über physikalische Zufallszahlen-Generatoren: https://en.wikipedia.org/wiki/Hardware_random_number_generator
