---
title:                "Написання тестів"
html_title:           "Elixir: Написання тестів"
simple_title:         "Написання тестів"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## Чому

Написання тестів є важливою частиною розробки програмного забезпечення, оскільки вони допомагають виявити та виправити помилки перед тим, як вони потраплять до продукційного коду. Це зберігає час та гроші на подальших корегуваннях та покращує якість програмного продукту.

## Як

Для написання тестів у Elixir, спочатку необхідно встановити пакет ExUnit, який є частиною стандартної бібліотеки. Після цього можна створювати тести для функцій та модулів, використовуючи вбудовані в Elixir методи, такі як `assert` та `refute`. Наприклад:

```Elixir
defmodule Math do
  def add(a, b) do
    a + b
  end
end

defmodule MathTest do
  use ExUnit.Case

  test "Додавання чисел" do
    assert Math.add(2, 3) == 5
  end
end
```

Виконання команди `mix test` запустить всі тести в проекті та поверне результати по кожному тесту.

## Глибоке погруження

Багато тестувальників стикаються з питанням, як писати ефективні тести. Деякі корисні підказки для написання якісних тестів включають:

- Створювати окремий файл для тестів кожного модуля
- Використовувати довгі та описові назви для тестів
- Писати тести для несподіваних та виняткових випадків
- Підтримувати тести оновленими при зміні коду

З якими викликами ви стикалися під час писання тестів у Elixir? Діліться своїм досвідом у коментарях нижче!

## Дивіться також

- [Dive Into Elixir Testing](https://medium.com/elixir-magic/dive-into-elixir-testing-while-learning-5b13df8e7bba)
- [ExUnit HexDocs](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Testing in Elixir](https://elixir-lang.org/getting-started/mix-otp/testing.html)