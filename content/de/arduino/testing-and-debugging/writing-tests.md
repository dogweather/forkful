---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:28.240710-07:00
description: "Tests im Arduino-Umfeld zu schreiben, bezieht sich auf den Prozess des\
  \ Erstellens von automatisierten Tests, die die Funktionalit\xE4t Ihres Codes auf\u2026"
lastmod: 2024-02-19 22:05:13.080900
model: gpt-4-0125-preview
summary: "Tests im Arduino-Umfeld zu schreiben, bezieht sich auf den Prozess des Erstellens\
  \ von automatisierten Tests, die die Funktionalit\xE4t Ihres Codes auf\u2026"
title: Tests Schreiben
---

{{< edit_this_page >}}

## Was & Warum?

Tests im Arduino-Umfeld zu schreiben, bezieht sich auf den Prozess des Erstellens von automatisierten Tests, die die Funktionalität Ihres Codes auf Arduino-Geräten validieren. Programmierer tun dies, um sicherzustellen, dass ihr Code wie erwartet funktioniert, Fehler reduziert und die Qualität ihrer Projekte verbessert, was besonders in eingebetteten Systemen, in denen das Debugging herausfordernder sein kann, von entscheidender Bedeutung ist.

## Wie:

Arduino verfügt nicht über ein eingebautes Test-Framework wie einige andere Programmierumgebungen. Sie können jedoch Drittanbieter-Bibliotheken wie `AUnit` für das Unit-Testing von Arduino-Code verwenden. AUnit ist inspiriert von der in Arduino integrierten Bibliothek `ArduinoUnit` und dem Test-Framework von Google, `Google Test`.

### Beispiel mit AUnit:

Installieren Sie zuerst AUnit über den Bibliotheks-Manager in der Arduino-IDE: gehen Sie zu Sketch > Bibliothek einbinden > Bibliotheken verwalten... > suchen Sie nach AUnit und installieren Sie es.

Danach können Sie Tests wie folgt schreiben:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // Leer
}
```
Nachdem Sie diesen Test auf Ihr Arduino-Board hochgeladen haben, öffnen Sie den Seriellen Monitor, um die Testergebnisse einzusehen. Sie sollten eine Ausgabe sehen, die angibt, ob jeder Test bestanden oder fehlgeschlagen ist:

```
TestRunner gestartet mit 2 Test(s).
Test ledPinHigh bestanden.
Test ledPinLow bestanden.
TestRunner Dauer: 0.002 Sekunden.
TestRunner Zusammenfassung: 2 bestanden, 0 fehlgeschlagen, 0 übersprungen, 0 abgelaufen, von 2 Test(s).
```

Dieses einfache Beispiel demonstriert die Verwendung von AUnit zum Testen des Zustands eines LED-Pins. Durch das Erstellen von Tests bestätigen Sie, dass sich Ihr Arduino unter verschiedenen Bedingungen wie erwartet verhält. Mit AUnit können Sie komplexere Tests, Test-Suiten erstellen und Funktionen wie Test-Timeouts und Einrichtungs-/Abbauverfahren für fortgeschrittenere Szenarien nutzen.
