---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:03:49.072553-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Регулярные выражения (regex) — это шаблоны, используемые для поиска комбинаций символов в тексте. Программисты используют их для задач, таких как проверка форматов, поиск и замена текста, а также разбор данных из сложных строк.

## Как это сделать:

В Elixir вы используете regex с встроенными шаблонами или создавая свои с помощью модуля `Regex`. Вот быстрый пример:

```elixir
# Поиск слова "hello"
regex = ~r/hello/
"hello world" =~ regex
# => true

# Поиск без учета регистра
regex = ~r/hello/i
"Hello world" =~ regex
# => true

# Замена "world" на "Elixir"
"hello world" |> String.replace(~r/world/, "Elixir")
# => "hello Elixir"
```

## Углубленный раздел

Регулярные выражения были изобретены в 1950-х годах математиком Стивеном Клини. Elixir реализует regex через библиотеку PCRE (Perl Compatible Regular Expressions), которая надежно сопоставляет шаблоны. Существуют альтернативы, такие как сопоставление строк с `String.contains?/2` или `String.starts_with?/2`, но они не обладают гибкостью, которую предлагает regex. Модуль `Regex` в Elixir компилирует шаблоны во внутренний формат, оптимизированный для повторного использования, экономя время вычислений.

## Смотрите также

- Документация по модулю `Regex` в Elixir: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Regex101, онлайн-тестер и отладчик regex: [https://regex101.com/](https://regex101.com/)
- "Программирование на Elixir" Дэйва Томаса - подробное руководство, которое также охватывает использование regex.
