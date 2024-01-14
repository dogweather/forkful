---
title:    "Elixir: Pisanie testów"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Dlaczego warto pisać testy w Elixirze

Testowanie kodu to niezwykle ważny element każdego projektu, a tworzenie testów w Elixirze może ułatwić proces weryfikacji kodu. Testy nie tylko pozwalają zidentyfikować błędy, ale również ułatwiają rozwój oprogramowania i dają poczucie pewności, że nasz kod działa poprawnie. Dlatego w dzisiejszym artykule opowiemy o tym, dlaczego warto pisać testy w Elixirze.

## Jak to zrobić

Najważniejszym elementem pisania testów w Elixirze jest wykorzystanie specjalnej składni, czyli `ExUnit`. Jest to wbudowany framework do testowania w Elixirze, który oferuje wiele funkcji ułatwiających pisanie testów.

Aby rozpocząć tworzenie testów, należy utworzyć nowy plik z rozszerzeniem `.exs` i umieścić w nim kod testów. Następnie należy zdefiniować moduł testowy poprzez użycie funkcji `defmodule` i wskazać, że wykorzystujemy `ExUnit` poprzez użycie funkcji `use ExUnit.Case`. Wtedy możemy rozpocząć definiowanie naszych testów.

Przykładowy kod testu w Elixirze może wyglądać następująco:

```Elixir
defmodule CalculatorTest do
  use ExUnit.Case

  test "addition" do
    result = Calculator.add(2, 2)
    assert result == 4
  end
end

```

Wywołanie testów odbywa się poprzez wywołanie funkcji `mix test` w terminalu. Otrzymamy wtedy informację o ilości testów, które zostały przeprowadzone oraz czy testy zakończyły się sukcesem czy też nie. Jeśli testy zakończą się niepowodzeniem, otrzymamy również informacje o błędach oraz wskazówki, jak je poprawić.

## Głębsza analiza

Pisanie testów w Elixirze może być prostsze, dzięki zastosowaniu funkcji `setup` i `setup_all`. Funkcja `setup` służy do wykonania kodu przed każdym testem, natomiast `setup_all` wykonuje kod tylko jeden raz przed wszystkimi testami. Jest to przydatne, gdy chcemy wykonać pewne przygotowania tylko raz, a nie przed każdym testem.

W Elixirze istnieje również możliwość tworzenia tzw. "mocków" czyli udawanych obiektów, które pomagają nam symulować różne scenariusze. Jest to szczególnie przydatne podczas testowania kodu, który korzysta z zewnętrznych zasobów, np. bazy danych czy plików.

Warto również wiedzieć, że Elixir oferuje możliwość testowania funkcji asynchronicznych, co jest niezwykle przydatne przy pracy z wątkami czy procesami.

# Zobacz również

Jeśli chcesz dowiedzieć się więcej o pisaniu testów w Elixirze, polecamy zapoznać się z dokumentacją ExUnit oraz wykorzystywać przykłady testów dostępne w oficjalnych repozytoriach Elixir. Poniżej zamieszczamy kilka przydatnych linków:

- Dokumentacja ExUnit: https://hexdocs.pm/ex_unit/readme.html
- Przykładowe testy: https://github.com/elixir-lang/elixir/tree/master/lib/ex_unit/test
- Poradnik dotyczący pisania testów w Elixirze: https://elixirschool.com/pl/lessons/advanced/testing/