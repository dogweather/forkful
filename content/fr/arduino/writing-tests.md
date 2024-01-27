---
title:                "Rédaction de tests"
date:                  2024-01-19
html_title:           "Arduino: Rédaction de tests"
simple_title:         "Rédaction de tests"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Écrire des tests, c'est vérifier que chaque morceau de code fait bien ce pour quoi il est conçu. Les développeurs le font pour prévenir les bugs, garantir la qualité et simplifier les mises à jour.

## Comment faire :

```Arduino
#include <ArduinoUnitTests.h>

void setup() {
  Serial.begin(9600);
}

void test_led_builtin_pin() {
  Test::assertEquals(HIGH, digitalRead(LED_BUILTIN), "LED should be HIGH");
}

void loop() {
  test_led_builtin_pin();
  Test::exclude("*");  // Pour éviter de tourner en boucle
}
```
Sortie échantillon:
```
Test led_builtin_pin: LED should be HIGH (1 tests, 1 passed, 0 failed, 0 skipped)
```

## Exploration approfondie

Historiquement, le test n'était pas une priorité dans les projet Arduino vu que c'était souvent des prototypes. Aujourd'hui, on a des frameworks comme ArduinoUnit pour une approche TDD (Test-Driven Development). Il y a aussi AUnit, une alternative. Les détails impliquent des mock-ups et des simulations pour le matériel.

## Voir aussi

- ArduinoUnit sur GitHub : https://github.com/mmurdoch/arduinounit
- Documentation AUnit : https://www.arduino.cc/reference/en/libraries/aunit/
- Article sur le TDD en C++ pour Arduino : https://www.arduino.cc/en/Guide/TestArduinoCode
