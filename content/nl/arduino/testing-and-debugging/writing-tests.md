---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:49.340125-07:00
description: "Testen schrijven betekent het cre\xEBren van een set voorwaarden om\
  \ te controleren of je code zich gedraagt zoals verwacht. Programmeurs doen dit\
  \ om fouten\u2026"
lastmod: '2024-03-13T22:44:51.075396-06:00'
model: gpt-4-0125-preview
summary: "Testen schrijven betekent het cre\xEBren van een set voorwaarden om te controleren\
  \ of je code zich gedraagt zoals verwacht. Programmeurs doen dit om fouten\u2026"
title: Tests Schrijven
weight: 36
---

## Wat & Waarom?
Testen schrijven betekent het creëren van een set voorwaarden om te controleren of je code zich gedraagt zoals verwacht. Programmeurs doen dit om fouten vroeg te ontdekken, softwarekwaliteit te garanderen, en hoofdpijn te voorkomen bij het toevoegen van nieuwe functies.

## Hoe te:
```Arduino
#include <Arduino.h>
#include <unity.h>

void setUp(void) {
// stel dingen hier in
}

void tearDown(void) {
// ruim hier op
}

void test_led_builtin_pin_nummer(void) {
    TEST_ASSERT_EQUAL(13, LED_BUILTIN);
}

void test_led_status_hoog(void) {
    digitalWrite(LED_BUILTIN, HIGH);
    TEST_ASSERT_EQUAL(digitalRead(LED_BUILTIN), HIGH);
}

void setup() {
    UNITY_BEGIN();
    RUN_TEST(test_led_builtin_pin_nummer);
    RUN_TEST(test_led_status_hoog);
    UNITY_END();
}

void loop() {
    // Meestal leeg bij testen
}
```
Uitvoer:
```
.
.
OK
```

## Diep Duiken
Historische context: Testen in Arduino kwam later dan in softwareontwikkeling en was minder gebruikelijk vanwege de interactie met hardware. Alternatieven: Handmatig testen, of meer complexe testraamwerken zoals Google Test. Implementatiedetails: Typisch gebruiken we een bibliotheek zoals ArduinoUnit of AUnit. Plaats testen in `setup()` en houd `loop()` leeg aangezien testen eenmalig draaien.

## Zie Ook
- ArduinoUnit bibliotheek: https://github.com/mmurdoch/arduinounit
- AUnit bibliotheek: https://github.com/bxparks/AUnit
- Introductie tot Unit Testing: https://www.arduino.cc/en/Guide/UnitTesting
