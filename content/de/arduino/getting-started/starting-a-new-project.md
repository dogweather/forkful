---
date: 2024-01-20 18:02:46.571107-07:00
description: "Ein neues Projekt zu starten bedeutet, eine leere Leinwand in der Arduino-IDE\
  \ zu \xF6ffnen und mit einer originellen Idee zu f\xFCllen. Programmierer tun dies\u2026"
lastmod: '2024-03-13T22:44:54.145390-06:00'
model: gpt-4-1106-preview
summary: "Ein neues Projekt zu starten bedeutet, eine leere Leinwand in der Arduino-IDE\
  \ zu \xF6ffnen und mit einer originellen Idee zu f\xFCllen."
title: Einen neuen Projekt starten
weight: 1
---

## How to:
Ein neues Projekt startet man mit einem leeren Sketch. Hier ein einfaches Blink-Programm als Ausgangspunkt:

```Arduino
// Definiere Pin für LED
const int ledPin = 13;

void setup() {
  // Konfiguriere den Pin als Ausgang
  pinMode(ledPin, OUTPUT);
}

void loop() {
  digitalWrite(ledPin, HIGH);   // LED einschalten
  delay(1000);                  // Warte eine Sekunde
  digitalWrite(ledPin, LOW);    // LED ausschalten
  delay(1000);                  // Warte eine Sekunde
}
```

Arduino-IDE: Bestätigung, dass das Programm hochgeladen wurde: "Hochladen abgeschlossen."

## Deep Dive
In der Arduino-Community ist es üblich, mit Blink-Beispielen zu beginnen. Historisch gesehen ist "Blink" das "Hallo Welt" der elektronischen Programmierung - ein einfacher Test, ob Hard- und Software korrekt kommunizieren. Alternativen sind Entwicklungsumgebungen wie PlatformIO oder auch Codebender, die zusätzliche Funktionen bieten können. Wichtig sind eine saubere Strukturierung des Codes und das Verstehen der Pin-Konfiguration und Timing-Funktionen, um Projekte effizient umsetzen zu können.

## See Also
- [Arduino Getting Started Guide](https://www.arduino.cc/en/Guide)
- [Arduino Language Reference](https://www.arduino.cc/reference/en/)
- [PlatformIO homepage](https://platformio.org/)
- [Codebender](https://codebender.cc/)
