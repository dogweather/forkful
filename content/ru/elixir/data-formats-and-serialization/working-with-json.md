---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:37.967809-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 JSON\
  \ \u0432 Elixir \u043C\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\
  \u043C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\
  \u043A\u0438\u0435 \u043A\u0430\u043A `Jason` \u0438\u043B\u0438 `Poison`. \u0412\
  \u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u0433\u0430\u0439\u0434\
  \ \u043F\u043E \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\
  \u0438\u044E `Jason`."
lastmod: '2024-03-13T22:44:44.470500-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0440\u0430\u0431\u043E\u0442\u044B \u0441 JSON \u0432\
  \ Elixir \u043C\u044B \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u043C\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0438, \u0442\u0430\u043A\
  \u0438\u0435 \u043A\u0430\u043A `Jason` \u0438\u043B\u0438 `Poison`."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
weight: 38
---

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
