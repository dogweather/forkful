---
title:                "Arduino: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego
Dobry testy są nie tylko ważne w Arduino, ale we wszystkich systemach programowania. Pomagają upewnić się, że twój kod działa poprawnie i pozwolić na łatwiejsze wykrywanie i naprawianie błędów.

## Jak to zrobić
Testy w Arduino mogą być wykonywane na różne sposoby, ale jeden z najczęściej stosowanych to użycie funkcji `assert`. Jest to funkcja, która sprawdza, czy podane wyrażenie jest prawdziwe, jeśli nie, to za pomocą funkcji `Serial.println()` można wypisać informację o błędzie. Oto przykładowy kod:

```Arduino
int x = 5;
assert(x == 5); //sprawdzanie czy x jest równy 5
Serial.println("Test passed!"); //jeśli tak, wypisuje informację o powodzeniu
```

Pamiętaj, że wartości boolean powinny być porównywane bezpośrednio, a nie za pomocą `if` lub `while`. Na przykład tak:

```Arduino
int y = 8;
assert(y > 7); //sprawdzenie czy y jest większe od 7
Serial.println("Test passed!");
```

## Dogłębne zagadnienia
Jedną z najważniejszych zasad pisania testów jest upewnienie się, że testy są niezależne od siebie. Oznacza to, że każdy test powinien sprawdzać tylko jedną rzecz i unikać interakcji z innymi testami. W ten sposób można łatwiej zlokalizować błąd, jeśli jakiś test nie przechodzi.

Nie zapominaj również o zakresie testów - należy sprawdzić zarówno przypadki prawidłowe, jak i nieprawidłowe, aby upewnić się, że twój kod jest odporny na błędy. Również nie zapomnij o testach jednostkowych, które sprawdzają pojedyncze fragmenty kodu, oraz testach integracyjnych, które sprawdzają, jak komponenty współpracują ze sobą.

## Zobacz również
- [Arduino - Oficjalna strona](https://www.arduino.cc/)
- [10 zasad pisania testów](https://medium.com/better-programming/10-principles-for-writing-good-tests-6382d8432f7c)
- [Testy jednostkowe i integracyjne w Arduino](https://www.hackster.io/jaydenjuejun/introducing-unit-and-integration-testing-in-arduino-a74876)