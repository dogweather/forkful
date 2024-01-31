---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"

category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Testowanie kodu to proces weryfikacji, czy nasze programy działają poprawnie. Programiści piszą testy, aby zapewnić niezawodność i unikać błędów podczas dodawania nowych funkcji.

## Jak to zrobić:
Arduino nie ma wbudowanego systemu do testów jednostkowych, ale możesz użyć biblioteki, np. ArduinoUnit. Przykładowy test:

```arduino
#include <ArduinoUnit.h>

test(ledBlinks) {
  pinMode(LED_BUILTIN, OUTPUT);
  digitalWrite(LED_BUILTIN, HIGH);
  assertTrue(digitalRead(LED_BUILTIN) == HIGH);
  digitalWrite(LED_BUILTIN, LOW);
  assertTrue(digitalRead(LED_BUILTIN) == LOW);
}

void setup() {
  Serial.begin(9600);
  Test::run();
}

void loop() {
}
```
Wynik:

```
Test ledBlinks: PASS
```

## Deep Dive:
Testowanie w Arduino jest relatywnie nowym zjawiskiem, a `ArduinoUnit` jest jedną z pierwszych bibliotek. Alternatywą może być `aunit`, bardziej zaawansowana, z zakresu asercji. Implementacja testów w Arduino często wymaga symulacji wejść/wyjść, ponieważ nie jest to typowe środowisko z automatyzacją.

## Zobacz także:
- [ArduinoUnit na GitHub](https://github.com/mmurdoch/arduinounit)
- [AUnit - ulepszony ArduinoUnit](https://github.com/bxparks/AUnit)
