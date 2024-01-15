---
title:                "Pisanie testów"
html_title:           "Elixir: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie testów jest nieodłączną częścią procesu tworzenia oprogramowania. Dzięki nim możemy upewnić się, że nasz kod działa poprawnie i nie wprowadza kolejnych błędów. Ponadto, testy pomagają w szybszym znajdowaniu i naprawianiu błędów oraz ułatwiają wprowadzanie zmian w kodzie.

## Jak to zrobić?

```elixir
defmodule Calculator do
  def add(a, b) do
    a + b
  end
end

defmodule CalculatorTest do
  use ExUnit.Case

  test "should add two numbers" do
    assert Calculator.add(2, 3) == 5
  end
end
```

W powyższym przykładzie tworzymy test jednostkowy dla prostej funkcji dodawania. Korzystając z modułu `ExUnit.Case`, definiujemy testy dla naszej aplikacji. Następnie w bloku `test` sprawdzamy czy wynik wywołania funkcji `Calculator.add` jest równy oczekiwanemu wynikowi - w tym przypadku `5`.

## Deep Dive

Pisanie dobrych testów to sztuka, która wymaga nie tylko znajomości składni języka, ale również umiejętności analizowania kodu i przewidywania możliwych błędów. Wymaga to także zrozumienia działania testów jednostkowych oraz innych narzędzi takich jak `ExUnit.Case` czy `mix test`. Warto również pamiętać o zasadzie "Test-Driven Development", czyli zasadzie pisania testów przed kodem, co pozwala zachować przejrzystość i poprawność naszego kodu.

## Zobacz też

- Oficjalna dokumentacja Elixir: https://hexdocs.pm/elixir
- Poradnik "Elixir School": https://elixirschool.com/pl/
- Kurs "Test Driven Development w Elixir": https://pragmaticstudio.com/courses/elixir