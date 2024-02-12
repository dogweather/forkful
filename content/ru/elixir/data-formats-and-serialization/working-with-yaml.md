---
title:                "Работа с YAML"
aliases:
- /ru/elixir/working-with-yaml.md
date:                  2024-01-29T00:05:07.876611-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/working-with-yaml.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
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
