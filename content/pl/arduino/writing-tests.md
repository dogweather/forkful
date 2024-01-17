---
title:                "Pisanie testów"
html_title:           "Arduino: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pisanie testów to proces, w którym programiści tworzą kod, który sprawdza poprawność działania ich programów. Jest to ważne, ponieważ pozwala zapewnić, że program będzie działać tak, jak powinien, bez nieoczekiwanych błędów.

## Jak to zrobić:

```Arduino
int pin = 13;

void setup() {
  pinMode(pin, OUTPUT);
}

void loop() {
  digitalWrite(pin, HIGH);
  delay(1000);
  digitalWrite(pin, LOW);
  delay(1000);
}
```

W tym przykładzie kodu ukazane jest, jak stworzyć prosty test, którym sprawdzamy, czy dioda na pinie 13 jest włączona i wyłączona co sekundę. Jeśli program działa poprawnie, powinniśmy obserwować takie zachowanie diody.

## Zagłębienie:

Pisanie testów jest najczęściej stosowaną metodą w programowaniu. Pozwala ona programistom zapewnić, że kod będzie działał poprawnie i uniknąć nieoczekiwanych błędów. Alternatywną metodą jest debugowanie, jednak testy są uważane za bardziej niezawodne i dające lepsze wyniki.

Pisanie testów jest również ważną częścią podejścia zwinnego w programowaniu. Pozwala to na ciągłe testowanie kodu i poprawianie ewentualnych błędów na bieżąco, co przyspiesza proces tworzenia oprogramowania.

Jeśli chcesz dowiedzieć się więcej o pisaniu testów w Arduino, możesz zajrzeć tutaj: https://www.arduino.cc/en/Guide/Introduction (ang.).

## Zobacz też:

- https://www.arduino.cc/en/Reference/TestsandMocking (ang.)
- https://medium.com/@devampro/test-driven-development-for-arduino-68460f36f4fe (ang.)