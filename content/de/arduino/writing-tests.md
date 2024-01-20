---
title:                "Tests schreiben"
html_title:           "Arduino: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Was & Warum?

Tests schreiben bedeutet, Code zu erstellen, der deinen eigenen Code prüft. Das machen Programmierer, um Fehler früh zu erkennen, die Stabilität des Programms zu garantieren und spätere Änderungen sicherer durchzuführen.

## How to:

Tests auf Arduino-Plattformen sind nicht so gängig wie in anderen Entwicklungsökosystemen, vor allem wegen des begrenzten Speichers und der Hardware-Fokus. Trotzdem kannst du ein einfaches Beispiel für eine Testfunktion nutzen, die eine LED blinken lässt, um eine „Hello World“-Funktionalität zu testen.

```Arduino
void setup() {
  pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
  testBlinkLED();
  delay(1000); // Warte eine Sekunde zwischen den Tests
}

void testBlinkLED() {
  digitalWrite(LED_BUILTIN, HIGH);   // LED an
  delay(1000);                       // Warte einen Moment
  digitalWrite(LED_BUILTIN, LOW);    // LED aus
  delay(1000);                       // Warte einen Moment
}

void assertLEDState(int expectedState) {
  int actualState = digitalRead(LED_BUILTIN);
  if (actualState != expectedState) {
    Serial.print("Test fehlgeschlagen: Erwarteter Status: ");
    Serial.print(expectedState);
    Serial.print(", Tatsächlicher Status: ");
    Serial.println(actualState);
  } else {
    Serial.println("Test erfolgreich!");
  }
}
```
Dieses einfache Testskript lässt eine eingebaute LED blinken und überprüft den Zustand.

## Deep Dive:

Arduino selbst bietet keine eingebauten Testframeworks wie andere Entwicklungsumgebungen. Historisch gesehen konzentriert sich die Arduino-Entwicklung eher auf direktes Ausprobieren (Trial-and-Error) und weniger auf automatisierte Tests. Alternativen wie das Unit-Test-Framework „AUnit“ sind verfügbar und bieten strukturierte Möglichkeiten, das Verhalten von Code zu überprüfen. Die Implementierung von Tests auf Arduino-Boards sollte berücksichtigen, dass Ressourcen sehr begrenzt sind – es geht darum, Minimaltests zu schreiben, die die Key-Funktionalitäten absichern.

## See Also:

- AUnit, ein Unit-Test-Framework für Arduino: https://github.com/bxparks/AUnit
- Arduinos offizielles Getting Started Guide: https://www.arduino.cc/en/Guide
- Eine Einführung in das Testen von Software allgemein: http://softwaretestingfundamentals.com/