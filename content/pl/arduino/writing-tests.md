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

## Dlaczego

Testowanie jest nieodłączną częścią procesu programowania i jest niezwykle ważne, aby zapewnić wysoką jakość kodu. Testy pozwalają nam sprawdzić poprawność działania naszego kodu oraz wykryć ewentualne błędy. Dzięki nim możemy być pewni, że nasza aplikacja działa zgodnie z oczekiwaniami.

## Jak To Zrobić

### Połączenie Arduino z komputerem

Przed przystąpieniem do pisania testów, musimy najpierw połączyć nasze Arduino z komputerem za pomocą kabla USB. Następnie musimy ustawić odpowiedni port szeregowy oraz model płytki w środowisku Arduino IDE.

### Instalacja biblioteki do testowania

Aby móc pisać testy, musimy zainstalować bibliotekę do testowania w naszym środowisku Arduino IDE. Jest to łatwe do zrobienia poprzez Menu "Sketch" > "Include Library" > "Manage Libraries...". Wyszukajmy bibliotekę "ArduinoUnit" i kliknijmy "Install".

### Przygotowanie kodu do testowania

Na początku naszego kodu musimy dodać dyrektywę `#include <ArduinoUnit.h>`, dzięki czemu będziemy mogli korzystać z funkcji testujących. Następnie musimy zdefiniować nasze testy w funkcji `setup()` przy użyciu funkcji `test()` i przekazać do niej nazwę naszego testu oraz kod do wykonania.

```Arduino
#include <ArduinoUnit.h>

void setup() {
  test("Nazwa_testu", [] {
    // Kod testowy
  });
}

void loop() {
  // Puste - nie jest potrzebne w przypadku pisania testów
}
```

### Uruchamianie testów

Aby uruchomić nasze testy, należy kliknąć przycisk "Verify" w środowisku Arduino IDE lub wybrać opcję "Verify/Compile" w menu "Sketch". Jeśli wszystko zostało poprawnie zdefiniowane, powinniśmy zobaczyć komunikat "OK" dla każdego z naszych testów w konsoli.

### Przykładowy test

Poniższy przykład pokazuje, jak możemy przetestować poprawne działanie funkcji `add()` dodającej dwie liczby.

```Arduino
#include <ArduinoUnit.h>

void setup() {
  test("Test dodawania", [] {
    assertEqual(add(2, 5), 7); // Sprawdza, czy funkcja add(2, 5) zwróci 7
  });
}

void loop() {
  // Puste - nie jest potrzebne w przypadku pisania testów
}

// Funkcja dodająca dwie liczby
int add(int a, int b) {
  return a + b;
}
```

## Deep Dive

Pisząc testy, powinniśmy pamiętać o kilku ważnych praktykach:

- Testy powinny być niezależne i niezależnie od siebie uruchamiane. Każdy test powinien testować tylko jeden konkretny aspekt naszego kodu.
- Używajmy funkcji `assert...()` do sprawdzania wyników w naszych testach. Funkcje te porównują otrzymany wynik z oczekiwanym i w przypadku niezgodności wyrzucają błąd.
- Testy powinny być napisane w taki sposób, aby były czytelne i łatwe do zrozumienia dla innych osób.
- Piszmy testy dla wszystkich funkcji naszego kodu, włączając w to również funkcje pomocnicze.

## Zobacz także

- Oficjalna dokumentacja biblioteki ArduinoUnit: https://github.com/mmurdoch/arduinounit#readme
- Przykładowe testy dla różnych funkcji w języku Arduino: https://github.com/tjentora/ArduinoTestExamples
- Poradnik wideo dotyczący pisania