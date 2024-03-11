---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:07.876611-07:00
description: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440 \u0438 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0434\
  \u0430\u043D\u043D\u044B\u0445 \u0432 \u0444\u043E\u0440\u043C\u0430\u0442\u0435\
  \ YAML, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u044F\u0432\u043B\u044F\u0435\
  \u0442\u0441\u044F \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043E\u043C\
  \ \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\
  \u0430\u043D\u043D\u044B\u0445, \u0447\u0438\u0442\u0430\u0435\u043C\u044B\u043C\
  \ \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u043E\u043C. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
lastmod: '2024-03-11T00:14:18.067565-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML \u0432\u043A\u043B\u044E\
  \u0447\u0430\u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0437\u0431\
  \u043E\u0440 \u0438 \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044E \u0434\
  \u0430\u043D\u043D\u044B\u0445 \u0432 \u0444\u043E\u0440\u043C\u0430\u0442\u0435\
  \ YAML, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u044F\u0432\u043B\u044F\u0435\
  \u0442\u0441\u044F \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043E\u043C\
  \ \u0441\u0435\u0440\u0438\u0430\u043B\u0438\u0437\u0430\u0446\u0438\u0438 \u0434\
  \u0430\u043D\u043D\u044B\u0445, \u0447\u0438\u0442\u0430\u0435\u043C\u044B\u043C\
  \ \u0447\u0435\u043B\u043E\u0432\u0435\u043A\u043E\u043C. \u041F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0438\u0441\u0442\u044B\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 YAML"
---

{{< edit_this_page >}}

## Что и Почему?

Работа с YAML включает в себя разбор и генерацию данных в формате YAML, который является стандартом сериализации данных, читаемым человеком. Программисты делают это для файлов конфигурации, обмена данными, и потому что это более читаемо, чем JSON или XML для сложных структур данных.

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
