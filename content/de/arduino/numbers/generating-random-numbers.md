---
date: 2024-01-27 20:32:44.592579-07:00
description: "Wie: Arduino bietet unkomplizierte Funktionen zur Erzeugung zuf\xE4\
  lliger Zahlen: `randomSeed()` und `random()`. Um zu beginnen, initialisieren Sie\
  \ den\u2026"
lastmod: '2024-03-13T22:44:54.140859-06:00'
model: gpt-4-0125-preview
summary: "Arduino bietet unkomplizierte Funktionen zur Erzeugung zuf\xE4lliger Zahlen."
title: Generierung von Zufallszahlen
weight: 12
---

## Wie:
Arduino bietet unkomplizierte Funktionen zur Erzeugung zufälliger Zahlen: `randomSeed()` und `random()`. Um zu beginnen, initialisieren Sie den Zufallszahlengenerator, um bei jedem Programmlauf verschiedene Zahlenfolgen zu gewährleisten. Ein oft genutzter Ansatz ist die Initialisierung mit einem Analogwert von einem nicht verbundenen Pin.

```Arduino
void setup() {
  Serial.begin(9600);
  // Zufallskeim initialisieren
  randomSeed(analogRead(0));
}

void loop() {
  // Eine zufällige Zahl zwischen 0 und 99 erzeugen
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Verzögerung für eine Sekunde zur besseren Lesbarkeit der Ausgabe
}
```

Das obige Programm initialisiert den Zufallszahlengenerator in der Funktion `setup()` und generiert in jeder Iteration der Schleife eine neue Zahl zwischen 0 und 99, die dann auf den Serial Monitor ausgegeben wird.

Beispielausgabe:
```
42
17
93
...
```

## Tiefergehend
Die Funktion `random()` von Arduino nutzt unter der Haube einen Pseudozufallszahlengenerator (PRNG), der einer deterministischen Sequenz folgt, aber statistisch zufällig aussieht. Der Anfangswert oder Keim der Sequenz hat einen großen Einfluss auf dessen Unvorhersehbarkeit, daher die übliche Verwendung von `randomSeed()` mit einem einigermaßen zufälligen Eingabewert als Ausgangspunkt. Es ist wichtig zu beachten, dass die von Arduino erzeugte Zufälligkeit für die meisten Hobbyprojekte ausreichend ist, aber möglicherweise nicht den Kriterien für hochsichere Anwendungen entspricht, aufgrund ihrer Vorhersehbarkeit über die Zeit. Für kryptografische Zwecke ist es ratsam, sich mit ausgefeilteren Algorithmen und Hardware-Zufallszahlengeneratoren (HRNGs) zu befassen, die durch die Nutzung physischer Prozesse echte Zufälligkeit bieten können.
