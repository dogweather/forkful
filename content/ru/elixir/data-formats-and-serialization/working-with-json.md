---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:04:37.967809-07:00
description: "JSON (JavaScript Object Notation) \u2014 \u044D\u0442\u043E \u043B\u0435\
  \u0433\u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\u0440\u043C\u0430\
  \u0442 \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\
  \u0438, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043B\u0435\u0433\u043A\u043E\
  \ \u0447\u0438\u0442\u0430\u0435\u0442\u0441\u044F \u0438 \u043F\u0438\u0448\u0435\
  \u0442\u0441\u044F \u043B\u044E\u0434\u044C\u043C\u0438, \u0430 \u0442\u0430\u043A\
  \u0436\u0435 \u043B\u0435\u0433\u043A\u043E \u0440\u0430\u0437\u0431\u0438\u0440\
  \u0430\u0435\u0442\u0441\u044F \u0438\u2026"
lastmod: '2024-03-11T00:14:18.069254-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u2014 \u044D\u0442\u043E \u043B\u0435\
  \u0433\u043A\u043E\u0432\u0435\u0441\u043D\u044B\u0439 \u0444\u043E\u0440\u043C\u0430\
  \u0442 \u043E\u0431\u043C\u0435\u043D\u0430 \u0434\u0430\u043D\u043D\u044B\u043C\
  \u0438, \u043A\u043E\u0442\u043E\u0440\u044B\u0439 \u043B\u0435\u0433\u043A\u043E\
  \ \u0447\u0438\u0442\u0430\u0435\u0442\u0441\u044F \u0438 \u043F\u0438\u0448\u0435\
  \u0442\u0441\u044F \u043B\u044E\u0434\u044C\u043C\u0438, \u0430 \u0442\u0430\u043A\
  \u0436\u0435 \u043B\u0435\u0433\u043A\u043E \u0440\u0430\u0437\u0431\u0438\u0440\
  \u0430\u0435\u0442\u0441\u044F \u0438\u2026"
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 JSON"
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
