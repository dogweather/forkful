---
title:                "Escrevendo testes"
date:                  2024-01-19
simple_title:         "Escrevendo testes"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Escrever testes é criar códigos que verificam se outras partes do seu programa funcionam como esperado. Programadores testam para evitar bugs, garantir a qualidade, e facilitar atualizações no futuro.

## Como Fazer:
```Arduino
#include <Arduino.h>
#include <unity.h>

void test_led_builtin_pin_number(void) {
    TEST_ASSERT_EQUAL(13, LED_BUILTIN);
}

void test_led_state_high(void) {
    digitalWrite(LED_BUILTIN, HIGH);
    TEST_ASSERT_EQUAL(HIGH, digitalRead(LED_BUILTIN));
}

void test_led_state_low(void) {
    digitalWrite(LED_BUILTIN, LOW);
    TEST_ASSERT_EQUAL(LOW, digitalRead(LED_BUILTIN));
}

void setup() {
    UNITY_BEGIN();
    RUN_TEST(test_led_builtin_pin_number);
    RUN_TEST(test_led_state_high);
    RUN_TEST(test_led_state_low);
    UNITY_END();
}

void loop() {
    // não é usado em testes
}
```
Saída Esperada:
```
.
.
.
```
(Três pontos indicam que todos os três testes passaram)

## Aprofundamento
Historicamente, escrever testes é uma prática vinda do desenvolvimento de software que tem sido adaptada para a programação em Arduino recentemente, graças ao surgimento de frameworks de teste, como a Unity. Alternativas para Arduino incluem AUnit e GoogleTest para projetos mais complexos. Detalhes importantes de implementação envolvem isolar o código a ser testado e simular o comportamento de hardware quando necessário.

## Veja Também
- [AUnit Library](https://www.arduino.cc/reference/en/libraries/aunit/)
