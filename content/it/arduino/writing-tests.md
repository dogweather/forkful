---
title:                "Scrivere test"
date:                  2024-01-19
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Scrivere test significa creare scenari automatizzati per verificare che il codice faccia esattamente quello che dovrebbe. I programmatori scrivono test per rilevare bug prima del rilascio e per mantenere il codice affidabile nel tempo.

## How to:
Esempio di un semplice test per un Arduino UNO che verifica il corretto lampeggio di un LED:

```arduino
#include <Arduino.h>
#include <unity.h>

void test_led_builtin_pin_number(void) {
    TEST_ASSERT_EQUAL(13, LED_BUILTIN);
}

void test_led_state_high(void) {
    digitalWrite(LED_BUILTIN, HIGH);
    TEST_ASSERT_EQUAL(digitalRead(LED_BUILTIN), HIGH);
}

void test_led_state_low(void) {
    digitalWrite(LED_BUILTIN, LOW);
    TEST_ASSERT_EQUAL(digitalRead(LED_BUILTIN), LOW);
}

void setup() {
    UNITY_BEGIN();
    pinMode(LED_BUILTIN, OUTPUT);
}

void loop() {
    RUN_TEST(test_led_builtin_pin_number);
    RUN_TEST(test_led_state_high);
    delay(1000);
    RUN_TEST(test_led_state_low);
    delay(1000);
    UNITY_END();
}
```

Il risultato sarà una serie di passaggi e fallimenti dei test sul monitor seriale.

## Deep Dive
Storicamente, nel mondo Arduino, i test non erano diffusi come nello sviluppo software classico a causa della natura fisica dell'hardware e delle complicazioni nel simulare l'ambiente. Oggi, con strumenti come la libreria Unity, possiamo implementare test unitari anche per i progetti Arduino. Alternative includono framework come Google Test per test più avanzati.

## See Also
Per approfondire l'argomento, visita:
- [Arduino Continuous Integration](https://github.com/ifreecarve/arduino_ci)
- [Unity Test Framework](http://www.throwtheswitch.org/unity)
