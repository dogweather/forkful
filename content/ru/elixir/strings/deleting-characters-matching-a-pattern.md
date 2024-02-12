---
title:                "Удаление символов, соответствующих шаблону"
aliases:
- /ru/elixir/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T23:56:53.487401-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление символов, соответствующих шаблону"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Удаление символов, соответствующих шаблону, заключается в поиске определённых последовательностей символов и их удалении. Программисты делают это для очистки данных, форматирования контента или манипулирования строками таким образом, чтобы они отвечали их конкретным потребностям.

## Как:

В Elixir используйте функцию `String.replace/4` для удаления символов, соответствующих шаблону. Посмотрите эти примеры:

```elixir
# Удалить цифры из строки
original_string = "Elixir2023Rocks!"
clean_string = String.replace(original_string, ~r/\d/, "")
IO.puts(clean_string) # Вывод: "ElixirRocks!"

# Удалить пунктуацию
punctuationless_string = String.replace(original_string, ~r/[[:punct:]]/, "")
IO.puts(punctuationless_string) # Вывод: "Elixir2023Rocks"

# Устранить пробелы
no_whitespace_string = String.replace(original_string, ~r/\s/, "")
IO.puts(no_whitespace_string) # Вывод: "Elixir2023Rocks!"
```

## Глубже в тему

Использование сопоставления с описанием для удаления символов в строках не является уникальным для Elixir; это общая возможность почти во всех языках программирования, эволюционировавшая из способностей регулярных выражений (regex) в ранних инструментах Unix, таких как `sed` и `grep`. Альтернативами `String.replace/4` могут быть использование сопоставления с образцом и рекурсии для ручного обхода и изменения строки, но этот метод обычно более многословен и сложен, что делает встроенные функции regex предпочтительными. Под капотом `String.replace/4` использует наследие Elixir от Erlang, используя мощные возможности сопоставления с образцом и манипулирования строками виртуальной машины BEAM.

## Смотрите также:

- Документация модуля `String` в Elixir: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Regex в Elixir: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- 'Изучите регулярные выражения': [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)
- Взгляд Elixir School на строки и сопоставление с образцом: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)
