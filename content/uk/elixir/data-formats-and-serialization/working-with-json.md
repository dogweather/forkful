---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:56.415111-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Elixir \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0431\u0456\
  \u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443 `Jason`, \u043F\u043E\u043F\u0443\
  \u043B\u044F\u0440\u043D\u0438\u0439 \u0432\u0438\u0431\u0456\u0440 \u0434\u043B\
  \u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 \u0442\u0430 \u0433\u0435\
  \u043D\u0435\u0440\u0430\u0446\u0456\u0457 JSON. \u0421\u043F\u043E\u0447\u0430\u0442\
  \u043A\u0443 \u0434\u043E\u0434\u0430\u0439\u0442\u0435 `Jason` \u0434\u043E\u2026"
lastmod: '2024-03-13T22:44:48.759458-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Elixir \u0432\u0438 \u043C\u043E\u0436\u0435\u0442\u0435 \u0432\u0438\
  \u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0431\
  \u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443 `Jason`, \u043F\u043E\u043F\
  \u0443\u043B\u044F\u0440\u043D\u0438\u0439 \u0432\u0438\u0431\u0456\u0440 \u0434\
  \u043B\u044F \u043F\u0430\u0440\u0441\u0438\u043D\u0433\u0443 \u0442\u0430 \u0433\
  \u0435\u043D\u0435\u0440\u0430\u0446\u0456\u0457 JSON."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
