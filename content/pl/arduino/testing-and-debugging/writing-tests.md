---
title:                "Pisanie testów"
date:                  2024-02-03T19:29:43.608105-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pisanie testów"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów w środowisku Arduino odnosi się do procesu tworzenia automatycznych testów, które weryfikują funkcjonalność twojego kodu na urządzeniach Arduino. Programiści robią to, aby upewnić się, że ich kod działa zgodnie z oczekiwaniami, zmniejsza liczbę błędów i poprawia jakość ich projektów, co jest szczególnie ważne w systemach wbudowanych, gdzie debugowanie może być bardziej wymagające.

## Jak to zrobić:

Arduino nie ma wbudowanego frameworka testowego, jak niektóre inne środowiska programistyczne. Możesz jednak użyć bibliotek stron trzecich, takich jak `AUnit` do testowania jednostkowego kodu Arduino. AUnit jest inspirowany wbudowaną biblioteką Arduino, `ArduinoUnit` oraz frameworkiem testowym Google, `Google Test`.

### Przykład z AUnit:

Najpierw zainstaluj AUnit przez Menedżera Bibliotek w IDE Arduino: przejdź do Sketch > Include Library > Manage Libraries... > wyszukaj AUnit i zainstaluj go.

Następnie możesz napisać testy w następujący sposób:

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
  // Puste
}
```
Po wgraniu tego testu na twoją płytę Arduino, otwórz Monitor Serialny, aby zobaczyć wyniki testu. Powinieneś zobaczyć wyniki wskazujące, czy każdy test zakończył się sukcesem czy porażką:

```
TestRunner rozpoczęty na 2 test(ach).
Test ledPinHigh zakończony sukcesem.
Test ledPinLow zakończony sukcesem.
Czas trwania TestRunner: 0.002 sekundy.
Podsumowanie TestRunner: 2 zakończone sukcesem, 0 nieudane, 0 pominięte, 0 przekroczone limity czasu, z 2 test(ów).
```

Ten prosty przykład pokazuje, jak używać AUnit do testowania stanu pinu LED. Tworząc testy, potwierdzasz, że twoje Arduino zachowuje się zgodnie z oczekiwaniami w różnych warunkach. Dzięki AUnit możesz pisać bardziej złożone testy, zestawy testów i korzystać z funkcji takich jak limity czasu testów oraz procedury przygotowania/likwidacji w bardziej zaawansowanych scenariuszach.