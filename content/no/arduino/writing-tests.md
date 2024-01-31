---
title:                "Skriving av tester"
date:                  2024-01-19
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Tester sjekker at koden din gjør det den skal, hver gang du gjør endringer. Det reduserer bugs og gir deg trygghet ved utvikling.

## Hvordan gjøre det:
Arduino støtter ikke enhetstesting ut av boksen, så vi bruker et bibliotek, for eksempel `ArduinoUnit`. Installer via bibliotekshåndterer og skriv testene dine.

```Arduino
#include <ArduinoUnit.h>

test(ledBlink) {
  const int LED_PIN = 13;
  pinMode(LED_PIN, OUTPUT);
  digitalWrite(LED_PIN, HIGH);
  assertTrue(digitalRead(LED_PIN));
  digitalWrite(LED_PIN, LOW);
  assertFalse(digitalRead(LED_PIN));
}

void setup() {
  Serial.begin(9600);
  Test::run();
}

void loop() {}
```
Kjør koden, og du ser resultatene i Serial Monitor:
```
Test ledBlink passed.
Test summary: 1 passed, 0 failed, and 0 skipped, out of 1 test(s).
```

## Dybdeinformasjon:
Testing på microcontrollere, som Arduino, startet mye senere enn i softwareutvikling. Alternativer inkluderer `AUnit`, `ArduinoTestSuite`, `CppUTest`. Disse bibliotekene simulerer enhetstester og krever at du skriver tester som verifiserer koden din uten å laste den på en faktisk mikrokontroller hver gang.

## Se også:
- [ArduinoUnit GitHub side](https://github.com/mmurdoch/arduinounit)
- [AUnit GitHub side](https://github.com/bxparks/AUnit)
- [Offisiell Arduino Testing Guide](https://www.arduino.cc/en/Guide/Environment#testing-libraries)
