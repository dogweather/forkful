---
title:                "Письмо тестів"
date:                  2024-02-03T19:30:47.082126-07:00
model:                 gpt-4-0125-preview
simple_title:         "Письмо тестів"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Написання тестів у Elixir полягає у створенні автоматизованих скриптів для перевірки поведінки вашого коду. Програмісти роблять це, щоб гарантувати якість, запобігати регресіям і полегшувати рефакторинг коду, роблячи процес розробки надійнішим та ефективнішим.

## Як це робити:
Elixir використовує ExUnit як вбудований тестовий фреймворк, який є надзвичайно потужним і легким у використанні. Ось базовий приклад:

1. Створіть новий тестовий файл у директорії `test` вашого проекту Elixir. Наприклад, якщо ви тестуєте модуль під назвою `MathOperations`, ваш тестовий файл може бути `test/math_operations_test.exs`.

```elixir
# test/math_operations_test.exs
defmodule MathOperationsTest do
  use ExUnit.Case

  # Це простий тестовий випадок для перевірки функції додавання
  test "додавання двох чисел" do
    assert MathOperations.add(1, 2) == 3
  end
end
```

Щоб запустити свої тести, використовуйте команду `mix test` у терміналі. Якщо функція `MathOperations.add/2` правильно додає два числа, ви побачите вивід, схожий на:

```
..

Завершено за 0.03 секунди
1 тест, 0 невдач
```

Для тестів, що включають зовнішні сервіси або API, ви можете захотіти використовувати мок-бібліотеки, такі як `mox`, щоб уникнути звернень до реальних сервісів:

1. Додайте `mox` до своїх залежностей у `mix.exs`:

```elixir
defp deps do
  [
    {:mox, "~> 1.0.0", only: :test},
    # інші залежності...
  ]
end
```

2. Визначте мок-модуль у вашому допоміжному тесті (`test/test_helper.exs`):

```elixir
Mox.defmock(HTTPClientMock, for: HTTPClientBehaviour)
```

3. Використовуйте мок у своєму тестовому випадку:

```elixir
# test/some_api_client_test.exs
defmodule SomeAPIClientTest do
  use ExUnit.Case
  import Mox

  # Це вказує Mox перевірити, що цей мок був викликаний як очікувалося
  setup :verify_on_exit!
  
  test "отримує дані з API" do
    # Налаштуйте мок-відповідь
    expect(HTTPClientMock, :get, fn _url -> {:ok, "Mocked response"} end)
    
    assert SomeAPIClient.get_data() == "Mocked response"
  end
end
```

Під час запуску `mix test`, цей підхід дозволяє вам ізолювати свої юніт-тести від реальних зовнішніх залежностей, зосереджуючись на поведінці вашого власного коду. Цей патерн гарантує, що ваши тести працюють швидко і залишаються надійними незалежно від статусу зовнішніх сервісів або Інтернет-з'єднання.
