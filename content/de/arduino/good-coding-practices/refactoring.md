---
date: 2024-01-26 01:16:45.489684-07:00
description: "Refactoring ist der Prozess der \xDCberarbeitung Ihres Codes, um seine\
  \ Struktur und Lesbarkeit zu verbessern, ohne das externe Verhalten oder die\u2026"
lastmod: '2024-03-13T22:44:54.151826-06:00'
model: gpt-4-0125-preview
summary: "Refactoring ist der Prozess der \xDCberarbeitung Ihres Codes, um seine Struktur\
  \ und Lesbarkeit zu verbessern, ohne das externe Verhalten oder die Funktionalit\xE4\
  t zu \xE4ndern."
title: Refactoring
weight: 19
---

## Was & Warum?
Refactoring ist der Prozess der Überarbeitung Ihres Codes, um seine Struktur und Lesbarkeit zu verbessern, ohne das externe Verhalten oder die Funktionalität zu ändern. Programmierer*innen betreiben Refactoring, um ihren Code sauberer, leichter verständlich und wartbarer zu machen, was auf lange Sicht das Debugging und das Hinzufügen neuer Funktionen erheblich erleichtert.

## Wie geht das:

Nehmen wir an, Sie haben eine Funktion auf Ihrem Arduino, die viel zu viel macht, so wie diese:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  // Eine Funktion, die zu viel macht
  handleEverything();
}

void handleEverything() {
  // Sensordaten lesen
  int sensorWert = analogRead(A0);
  // Die Sensordaten verarbeiten
  sensorWert = map(sensorWert, 0, 1023, 0, 255);
  // Die Sensordaten ausgeben
  Serial.println(sensorWert);
  delay(500);
}
```

Das Refactoring könnte darin bestehen, `handleEverything()` in kleinere, fokussiertere Funktionen aufzuteilen:

```Arduino
void setup() {
  Serial.begin(9600);
}

void loop() {
  int sensorWert = readSensorData();
  int verarbeiteterWert = processSensorData(sensorWert);
  printData(verarbeiteterWert);
  delay(500);
}

int readSensorData() {
  return analogRead(A0);
}

int processSensorData(int sensorWert) {
  return map(sensorWert, 0, 1023, 0, 255);
}

void printData(int daten) {
  Serial.println(daten);
}
```

Nach dem Refactoring ist die `loop()`-Funktion lesbarer und jede Aufgabe wird von einer dedizierten Funktion gehandhabt, was den Code einfacher zu verwalten macht.

## Tiefer eintauchen
Historisch gesehen wurde Refactoring populär mit dem Aufstieg von Agilen und Testgetriebenen Entwicklung (TDD) Methodologien, die sich auf kontinuierliche Codeverbesserungen verlassen, um sich an wechselnde Anforderungen anzupassen. Es gibt verschiedene Werkzeuge und Strategien für Refactoring – wie die Technik "Methode extrahieren", die wir in unserem Arduino-Beispiel verwendet haben. Dies ist essentiell, wenn Sie von einem schnellen Prototypen zu einem stabilen Projekt übergehen, bei dem Codelesbarkeit und -wartung entscheidend werden.

Beim Refactoring ist es wichtig, einen guten Satz an Tests zur Verfügung zu haben, um sicherzustellen, dass Änderungen keine Fehler eingeführt haben. In der Arduino-Welt ist das automatisierte Testen aufgrund von Hardwareabhängigkeiten nicht immer unkompliziert, aber Sie können immer noch Unit-Tests für reine Logikteile verwenden oder Simulatoren einsetzen.

Alternativen zum manuellen Refactoring umfassen die Verwendung dedizierter Refactoring-Tools, welche die Identifizierung von „Code-Gerüchen“ automatisieren und Änderungen vorschlagen. Diese Werkzeuge fehlen jedoch oft an Feingefühl für Mikrocontroller-Code und sind möglicherweise nicht in der Arduino-Entwicklungsumgebung verfügbar.

Letztendlich ist Refactoring eine Kunst, die das Verbessern der internen Struktur des Codes gegen das Risiko, Fehler einzuführen, abwägt. Es verlangt von Ihnen, über Implementierungsdetails wie Speichernutzung und Prozessorzeit nachzudenken, insbesondere aufgrund der ressourcenbeschränkten Natur von Mikrocontrollern.

## Siehe auch
Für einen tieferen Einblick in Refactoring lohnt sich das wegweisende Buch von Martin Fowler *Refactoring: Improving the Design of Existing Code*. Für einen genaueren Blick auf Arduino-spezifische Praktiken, besuchen Sie die Arduino-Entwicklungsforen und -gemeinschaften:

- [Arduino Forum - Programmierfragen](https://forum.arduino.cc/index.php?board=4.0)
- [Refactoring Guru](https://refactoring.guru/refactoring)

Denken Sie daran, das Ziel ist sauberer, verständlicher Code, für den Sie, die zukünftige Sie und andere, dankbar sein werden. Bleiben Sie am Ball und halten Sie es ordentlich!
