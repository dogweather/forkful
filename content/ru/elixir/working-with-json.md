---
title:                "Работа с JSON"
date:                  2024-01-29T00:04:37.967809-07:00
model:                 gpt-4-0125-preview
simple_title:         "Работа с JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/working-with-json.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
JSON (JavaScript Object Notation) — это легковесный формат обмена данными, который легко читается и пишется людьми, а также легко разбирается и генерируется машинами. Программисты работают с JSON для обмена данными между серверами и веб-приложениями, хранения конфигурации или сериализации данных для сетевого общения.

## Как это сделать:

Для работы с JSON в Elixir мы используем библиотеки, такие как `Jason` или `Poison`. Вот быстрый гайд по использованию `Jason`:

```elixir
# Добавьте Jason в ваш mix.exs как зависимость
{:jason, "~> 1.3"}

# в файле .ex для кодирования Elixir в JSON
json_string = Jason.encode!(%{foo: "bar"})

# Теперь декодирование JSON в Elixir
elixir_map = Jason.decode!(json_string)
```

Вывод:

```elixir
json_string #=> "{\"foo\":\"bar\"}"
elixir_map  #=> %{"foo" => "bar"}
```

Кодирование с `opts` для красивого вывода:

```elixir
Jason.encode!(%{foo: "bar"}, pretty: true)
```

Вывод:

```json
{
  "foo": "bar"
}
```

## Глубже в тему

JSON был предложен Дугласом Крокфордом в начале 2000-х. Он быстро получил распространение из-за своей простоты по сравнению с XML.

Альтернативы? Конечно — XML, YAML или Protocol Buffers, но JSON остается в лидерах из-за простоты и нативной поддержки в JavaScript.

Под капотом библиотеки JSON преобразуют типы данных Elixir в строки JSON и обратно. Сопоставление с образцом в Elixir и мощная стандартная библиотека делают процесс кодирования и декодирования плавным.

## Смотрите также

- Jason GitHub: https://github.com/michalmuskala/jason
- Poison GitHub: https://github.com/devinus/poison
- Уроки JSON на школе Elixir: https://elixirschool.com/en/lessons/specifics/jason/
