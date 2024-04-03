---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:07.876611-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Elixir \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442\
  \ \u0432 \u0441\u0435\u0431\u044F \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\
  \u0443\u044E \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0443 YAML, \u043D\
  \u043E \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0443 `yamerl`. \u0421\u043D\u0430\u0447\u0430\u043B\u0430 \u0434\
  \u043E\u0431\u0430\u0432\u044C\u0442\u0435 `yamerl` \u0432 \u0432\u0430\u0448 \u0444\
  \u0430\u0439\u043B\u2026"
lastmod: '2024-03-13T22:44:44.468905-06:00'
model: gpt-4-0125-preview
summary: "Elixir \u043D\u0435 \u0432\u043A\u043B\u044E\u0447\u0430\u0435\u0442 \u0432\
  \ \u0441\u0435\u0431\u044F \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u0443\
  \u044E \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0443 YAML, \u043D\u043E\
  \ \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\
  \u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\
  \u0435\u043A\u0443 `yamerl`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
weight: 41
---

## Как это сделать:
Elixir не включает в себя встроенную поддержку YAML, но вы можете использовать библиотеку `yamerl`. Сначала добавьте `yamerl` в ваш файл `mix.exs`:

```elixir
defp deps do
  [{:yamerl, "~> 0.8"}]
end
```

После выполнения `mix deps.get`, вы можете разобрать YAML:

```elixir
yml_data = """
name: John Doe
age: 30
langs:
  - Elixir
  - Ruby
  - Haskell
"""

parsed_data = :yamerl_constr.string(yml_data) |> Enum.take(1)
IO.inspect(parsed_data)
```

Это выведет:

```elixir
[
  %{
    "age" => 30,
    "langs" => ["Elixir", "Ruby", "Haskell"],
    "name" => "John Doe"
  }
]
```

И для конвертации данных Elixir в YAML:

```elixir
data = %{
  name: "John Doe",
  age: 30,
  langs: ["Elixir", "Ruby", "Haskell"]
}

yml_string = :yamerl.encode(data)
IO.puts yml_string
```

Это напечатает:

```yaml
---
age: 30
langs:
  - Elixir
  - Ruby
  - Haskell
name: John Doe
```

## Подробнее
YAML, что означает "YAML Ain't Markup Language" (рекурсивный акроним), существует с 2001 года. JSON и XML могут служить похожим целям, но фокус YAML на читаемости делает его популярным для конфигураций. `yamerl`, библиотека Erlang, адаптированная для Elixir через взаимодействие, является надежным выбором для разработчиков на Elixir. Помните, что YAML чувствителен к отступам, что делает разбор немного сложнее по сравнению с JSON.

## Смотрите также
- Официальный репозиторий `yamerl` на GitHub: https://github.com/yakaz/yamerl
- `hexdocs` для Elixir по библиотекам YAML: https://hex.pm/packages?search=yaml&sort=recent_downloads
- Официальный сайт YAML для спецификаций и многого другого: https://yaml.org
- Elixir School для изучения Elixir: https://elixirschool.com/en/
