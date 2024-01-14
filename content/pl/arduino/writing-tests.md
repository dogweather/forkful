---
title:    "Arduino: Pisanie testów"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Dlaczego piszemy testy w Arduino?

Witajcie, drodzy czytelnicy! Jeśli zajmujecie się programowaniem w Arduino, z pewnością wiecie, że jest to proces wymagający precyzji i dokładności. Aby mieć pewność, że nasz kod działa poprawnie, warto zastosować testy jednostkowe. Dlaczego? Ponieważ umożliwiają one szybkie wykrycie błędów i ułatwiają utrzymanie czystości kodu. W tym artykule pokażemy Wam, jak pisać testy w Arduino i jakie korzyści to przynosi.

## Jak pisać testy w Arduino?

Aby przetestować nasz kod, potrzebujemy odpowiednich narzędzi. W Arduino jest to biblioteka o nazwie `ArduinoUnit`, która umożliwia nam pisanie i uruchamianie testów jednostkowych. Najpierw musimy ją zainstalować, a następnie zaimportować do naszego projektu. Poniżej przedstawimy przykładowy kod i jego wyjście, abyście mogli lepiej zrozumieć, jak działa `ArduinoUnit`.

```Arduino
#include <ArduinoUnit.h> // importujemy bibliotekę

// funkcja testowa
void testSum() {
  int a = 2;
  int b = 3;
  int expected = 5;

  int result = a + b;
  assertEquals(expected, result); // sprawdzamy, czy otrzymany wynik jest taki sam jak oczekiwany
}

// funkcja wywoływana na początku wykonania testów
void setup() {
  Test::run(report); // uruchamiamy testy i przekazujemy funkcję 'report', która wypisze wyniki
}

// funkcja wywoływana po zakończeniu testów
void tearDown() {
  // możemy w niej np. zwolnić zasoby zajmowane w testach
}

// główna pętla programu, nie musimy jej implementować
void loop() {}

// funkcja wykorzystywana przez 'Test::run' do raportowania wyników
void report(int testCount, int failureCount) {
  // opcjonalnie możemy wypisać ilość testów i błędów
  // oraz ich szczegóły, jeśli takie się pojawiły
  // w przypadku braku błędów wypisze 'OK'
  Test::printTestResults();
}
```
Obserwując powyższy kod i jego wyjście, możemy zauważyć, że testy są bardzo czytelne i możemy szybko zobaczyć, czy wszystko działa poprawnie. Ponadto, dzięki asercjom, nie musimy samodzielnie porównywać oczekiwanego wyniku z otrzymanym. To zdecydowanie ułatwia pracę i pozwala odpowiednio wcześniej wykryć błędy.

## Głębszy zanurk wiadomości

Testy jednostkowe to nie tylko narzędzie do wykrywania błędów, ale również metoda na poprawę jakości naszego kodu. Poprzez wydzielenie poszczególnych funkcji i testowanie ich osobno, możemy uniknąć problemów z późniejszą integracją i osiągnąć wyższą czytelność i przejrzystość kodu. Ponadto, dzięki testom jednostkowym, mamy pewność, że wprowadzone przez nas zmiany nie wpłynęły negatywnie na działanie pozostałych części programu.

## Zobacz również

- [Dokumentacja ArduinoUnit](https://arduinounit.github.io/)
- [Artykuł o testowaniu w Arduino na blogu Adafruit](https://blog.adafruit.com/2009/08/18/testing-your-arduino-code-arduino-unit/)
- [Przykładowy projekt z wykorzystaniem testów w Arduino](https://github.com/ivankravets/Arduino