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

Co i dlaczego?
Testowanie jest procesem pisania kodu, który pomaga programistom weryfikować, czy ich kod działa poprawnie. Jest to często wykonywane przez programistów, aby zapewnić, że ich aplikacja lub system działa zgodnie z oczekiwaniami.

Jak to zrobić:
Elixir dostarcza bibliotekę o nazwie ExUnit, która jest wykorzystywana do tworzenia testów w języku Elixir. Poniżej znajdują się przykład kodu oraz wyjście, aby pomóc Ci zacząć pisać testy.

```
defmodule Calculator do
  def add(x, y) do
    x + y
  end
end

defmodule CalculatorTest do
  use ExUnit.Case

  test "should add two numbers" do
    result = Calculator.add(2, 3)
    assert result == 5
  end
end
```

```
$ elixir calculator_test.exs
...

Finished in 0.03 seconds
1 test, 0 failures
```

Głębsze spojrzenie:
Historia testowania sięga lat 50-tych, kiedy to zapoczątkowano praktykę pisania testów automatycznych w języku assembler. Alternatywą dla pisania testów w Elixir jest wykorzystanie bibliotek takich jak ExUnit czy Doctests, które również wspierają testy jednostkowe.

Zobacz również:
- Dokumentacja biblioteki ExUnit: https://hexdocs.pm/ex_unit/ExUnit.html
- Tutorial "Testowanie w Elixir": https://elixir-lang.org/getting-started/testing-with-exunit.html 
- Porównanie różnych bibliotek do testowania w Elixir: https://medium.com/@SundayAKpan/test-in-elixir-using-exunit-vs-dialyxir-4b4ac2d128a0