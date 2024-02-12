---
title:                "Робота з JSON"
aliases:
- uk/elixir/working-with-json.md
date:                  2024-02-03T19:22:56.415111-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та чому?

Робота з JSON передбачає парсинг рядків у форматі JSON до структур даних, які Elixir може маніпулювати, та серіалізацію структур даних Elixir назад у рядки JSON. Це невід'ємно для веб-розробки, API та файлів конфігурації, оскільки JSON є легковісним, текстовим, незалежним від мови форматом обміну даними, який широко використовується за свою простоту та зручність для людського сприйняття.

## Як це зробити:

У Elixir ви можете використовувати бібліотеку `Jason`, популярний вибір для парсингу та генерації JSON. Спочатку додайте `Jason` до залежностей вашого проекту в `mix.exs`:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

Потім запустіть `mix deps.get` для отримання залежності.

### Парсинг JSON:
Для конвертації рядка JSON у структури даних Elixir:

```elixir
json_string = "{\"name\":\"Іван\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# Вивід: %{"name" => "Іван", "age" => 30}
```

### Генерація JSON:
Для конвертації мапи Elixir у рядок JSON:

```elixir
person_map = %{"name" => "Олена", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# Вивід: {"age":25,"name":"Олена"}
```

### Робота зі структурами:
Для кодування структури Elixir необхідно реалізувати протокол `Jason.Encoder` для вашої структури. Ось приклад:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Михайло", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# Вивід: {"age":28,"name":"Михайло"}
```

Цей простий підхід допоможе вам розпочати інтеграцію обробки JSON у ваші застосунки Elixir, сприяючи обміну даними у різноманітних програмних середовищах.
