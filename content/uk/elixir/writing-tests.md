---
title:                "Написання тестів"
html_title:           "Arduino: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Що це таке і чому?
Тести - це частина коду, що перевіряє чи ваша програма працює правильно. Програмісти пишуть тести, щоб автоматизувати перевірку функціональності та уникнути помилок у майбутньому.

## Як це робити:
```elixir
# Створення простого тесту в Elixir за допомогою ExUnit
defmodule ExampleTest do
  use ExUnit.Case
  doctest Example

  # Тестовий приклад
  test "the truth" do
    assert 1 + 1 == 2
  end
end
```
Виконання: `$ mix test`
Вивід: `1 test, 0 failures`

## Поглиблено:
Тестування в Elixir базується на ExUnit, що з'явився разом з мовою в 2011 році. Альтернатив ExUnit немає, але є можливість інтеграції з іншими інструментами, наприклад, с ExVCR для тестування HTTP запитів. Механіка тестування базується на патерни "setup/teardown" для налаштування середовища перед кожним тестом.

## Дивіться також:
- [Elixir School на українському](https://elixirschool.com/uk/)
- [Офіційна документація ExUnit](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir Testing Guidelines](https://github.com/christopheradams/elixir_style_guide#testing)