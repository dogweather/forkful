---
title:                "Elixir: Написання тестів"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/writing-tests.md"
---

{{< edit_this_page >}}

"## Чому"

Написання тестів - це важлива частина процесу програмування на Elixir. Вони допомагають перевірити правильність роботи коду, заощаджуючи час і зусилля у майбутньому.

## Як це зробити
```
Elixir defmodule Test do
  def add(x, y) do
    x + y
  end
end
```
Використовуючи вбудований інструмент ExUnit, можна легко написати тест для цього модуля:
```
Elixir defmodule Test do
  use ExUnit.Case

  test "додає два числа" do
    assert Test.add(1, 2) == 3
  end
end
```
Запустіть тест з командного рядка за допомогою `mix test`, і ви отримаєте наступний результат:
```
Elixir..............................

Finished in 0.3 seconds
28 tests, 0 failures

Randomized with seed 123456
```

## Глибоке дослідження
Написання тестів в Elixir надає значну перевагу у порівнянні з іншими мовами програмування. Оскільки Elixir заснований на функціональному підході, функції є чистими та не мають побічних ефектів, що робить їх легко тестованими. Крім того, присутність вбудованого Reproducible Ramdom Generator у ExUnit дозволяє запускати тести у будь-якому порядку, що робить їх більш надійними.

## Дивіться також
- [Elixir School](https://elixirschool.com/uk/)
- [Elixir Forum](https://elixirforum.com/c/elixir-questions)
- [ExUnit документація](https://hexdocs.pm/ex_unit/ExUnit.html)