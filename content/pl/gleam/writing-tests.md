---
title:    "Gleam: Pisanie testów"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego?

Pisanie testów jest kluczowym elementem w tworzeniu niezawodnego oprogramowania. Testy pozwalają nam na weryfikację kodu i zapewnienie, że działa on zgodnie z oczekiwaniami. Bez testów, wprowadzanie zmian w kodzie może być niebezpieczne, ponieważ może spowodować nieoczekiwane błędy. Dlatego ważne jest, aby programiści angażowali się w pisanie testów, aby poprawić jakość swojego kodu.

## Jak to zrobić?

Aby pisać testy w Gleam, musimy najpierw zaimportować moduł `gleam/testing` do naszego pliku. Następnie możemy użyć funkcji `suite`, aby zdefiniować testy w naszej funkcji głównej. Poniżej przedstawiono przykładowy kod:

```Gleam
import gleam/testing

suite "Kalkulator" {
  test "Dodawanie" {
    assert.equal(2, 1 + 1)
  }
  test "Mnożenie" {
    assert.equal(4, 2 * 2)
  }
}
```

Wywołując funkcję `suite` z nazwą testów oraz funkcjami `test` i `assert.equal`, możemy przetestować różne aspekty naszego kodu. Po uruchomieniu testów, jeśli wynik jest zgodny z oczekiwaniami, zobaczymy komunikat "Pass", a jeśli jest inny, zobaczymy błąd i otrzymamy informacje o błędzie. Możemy również wywoływać testy w funkcjach lub przez żądania HTTP.

## Deep Dive

Istnieje wiele różnych sposobów, aby pisać testy w Gleam, w zależności od tego, jakie aspekty kodu chcemy przetestować. Na przykład możemy użyć funkcji `assert.true` lub `assert.false`, aby sprawdzić, czy wyrażenie jest prawdziwe lub fałszywe, lub `assert.contains`, aby sprawdzić, czy dana lista zawiera dany element. Możliwości są nieograniczone, dlatego warto eksperymentować i znaleźć najlepsze metody dla swojego kodu.

## Zobacz także

- Dokumentacja Gleama o pisaniu testów: [link](https://gleam.run/book/tutorials-and-guides/unit-testing.html)
- Przykładowe testy w Gleam: [link](https://github.com/gleam-lang/gleam/blob/master/lib/gleam_testing/test/test.gleam)