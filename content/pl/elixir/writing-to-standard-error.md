---
title:                "Elixir: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest ważnym aspektem programowania w Elixirze. Wydaje się to być tylko jednym z wielu sposobów na przekazywanie informacji o błędach w naszym kodzie, ale jest to również metoda, która może pomóc nam w diagnozowaniu problemów i poprawianiu naszej pracy. W tej krótkiej instrukcji dowiesz się, dlaczego pisanie do standardowego błędu jest ważne i jak zacząć to robić.

## Jak to zrobić

Aby napisać do standardowego błędu w Elixirze, należy użyć funkcji `IO.puts/2` lub `IO.puts/1` w celu wyświetlenia błędu na ekranie. Przykłady kodu:

```Elixir
IO.puts("To jest przykładowy błąd!")
```

```Elixir
IO.puts("Błąd numer", 404)
```

Potrzebujemy również wywołać funkcję `IO.inspect/1` na żądanym typie danych, aby wyświetlić szczegółowe informacje o błędzie. Przykład:

```Elixir
IO.inspect(4/0, label: "dzielenie przez 0:")
```

Output:

```Elixir
dzielenie przez 0: %{__exception__: %Error,
                         __struct__: %ZeroDivisionError{},
                         left: 4,
                         right: 0}
```

Możemy również użyć renderowania wyjątków za pomocą makr `try/rescue/1` w celu przechwytywania błędów i wyświetlania ich na ekranie. Przykład:

```Elixir
try do
  4/0
rescue
  e in Error ->
    IO.puts(e.message)
end
```

Output:

```Elixir
"division by zero"
```

## Głębsza analiza

Pisanie do standardowego błędu jest ważnym narzędziem w Elixirze nie tylko ze względu na wyświetlanie błędów, ale także dla łatwiejszego debugowania i analizowania naszego kodu. Wykorzystywanie funkcji `IO.puts/2`, `IO.puts/1` i `IO.inspect/1` pozwala nam na wyświetlanie naszych własnych wiadomości błędów, dodawanie etykiet i renderowanie wyjątków w miejscach, gdzie uważamy to za potrzebne.

## Zobacz również

- [Dokumentacja Elixir IO](https://hexdocs.pm/elixir/IO.html)
- [Przewodnik po debugowaniu w Elixirze](https://elixirschool.com/pl/lessons/advanced/debugging/)