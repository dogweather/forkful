---
title:                "Elixir: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego pisać testy w Elixirze?

 Testy są nieodłączną częścią procesu programowania. Pozwalają nam upewnić się, że nasz kod nie tylko działa poprawnie, ale także jest przygotowany na zmiany i refaktoring w przyszłości. W Elixirze pisane są w sposób wyjątkowo intuicyjny i łatwy do zrozumienia, dlatego warto poznać podstawy, aby w przyszłości móc z niego korzystać.

## Jak pisać testy w Elixirze?

Poniżej przedstawione są przykładowe kody z funkcjami oraz odpowiedziami jakie można uzyskać po uruchomieniu testów:

```Elixir
defmodule Calculator do
  def add(x, y) do
    x + y
  end
end
```

```Elixir
# Test
test "dodawanie dwoch liczb" do
  assert Calculator.add(2, 3) == 5
end
```

```Elixir
# Output
Compiling 1 file (.ex)
..

Finished in 0.03 seconds
1 test, 0 failures
```

Używając funkcji `test` możemy jednoznacznie określić, co chcemy przetestować. W parametrze podajemy nazwę testu, a w ciele funkcji można wykorzystać funkcję `assert`, która sprawdza, czy wynik zwrócony przez naszą funkcję jest taki sam jak oczekiwany.

## Deep Dive: Co więcej warto wiedzieć o pisaniu testów w Elixirze?

Poza standardowymi testami jednostkowymi, Elixir oferuje również narzędzie do tworzenia testów integracyjnych - `ExUnit.CaseIntegration`. Pozwala ono na testowanie zależności między modułami oraz komunikację między procesami. Warto także wspomnieć o asercjach dynksyjnych, które pozwalają na sprawdzanie kodu obsługującego błędy oraz tzw. "flaky tests", czyli testów, które nie zawsze zwracają ten sam wynik, a jednak są prawidłowe.

## Zobacz także

- [Dokumentacja Elixir](https://elixir-lang.org/docs.html)
- [Blog programistyczny](https://blogprogramistyczny.pl/pisanie-testow-w-elixirze/)
- [Testy w Elixirze w praktyce](https://solidsoft.wordpress.com/2013/08/21/testy-w-elixirze-w-praktyce/)