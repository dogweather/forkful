---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:56.415111-07:00
description: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON \u043F\u0435\u0440\u0435\
  \u0434\u0431\u0430\u0447\u0430\u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u0443 \u0444\u043E\u0440\u043C\u0430\u0442\
  \u0456 JSON \u0434\u043E \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0434\
  \u0430\u043D\u0438\u0445, \u044F\u043A\u0456 Elixir \u043C\u043E\u0436\u0435 \u043C\
  \u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u0442\u0438, \u0442\u0430\
  \ \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u044E \u0441\
  \u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0434\u0430\u043D\u0438\u0445 Elixir\
  \ \u043D\u0430\u0437\u0430\u0434 \u0443\u2026"
lastmod: '2024-03-13T22:44:48.759458-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON \u043F\u0435\u0440\u0435\
  \u0434\u0431\u0430\u0447\u0430\u0454 \u043F\u0430\u0440\u0441\u0438\u043D\u0433\
  \ \u0440\u044F\u0434\u043A\u0456\u0432 \u0443 \u0444\u043E\u0440\u043C\u0430\u0442\
  \u0456 JSON \u0434\u043E \u0441\u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0434\
  \u0430\u043D\u0438\u0445, \u044F\u043A\u0456 Elixir \u043C\u043E\u0436\u0435 \u043C\
  \u0430\u043D\u0456\u043F\u0443\u043B\u044E\u0432\u0430\u0442\u0438, \u0442\u0430\
  \ \u0441\u0435\u0440\u0456\u0430\u043B\u0456\u0437\u0430\u0446\u0456\u044E \u0441\
  \u0442\u0440\u0443\u043A\u0442\u0443\u0440 \u0434\u0430\u043D\u0438\u0445 Elixir\
  \ \u043D\u0430\u0437\u0430\u0434 \u0443\u2026"
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
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
