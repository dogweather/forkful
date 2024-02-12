---
title:                "Преобразование строки в верхний регистр"
aliases:
- /ru/elixir/capitalizing-a-string/
date:                  2024-01-28T23:55:50.106550-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Сделать первую букву строки заглавной – значит изменить первую букву данной строки на заглавную, если это буква. Программисты делают это для обеспечения единообразия форматирования, улучшения интерфейса пользователя или соблюдения стандартов данных.

## Как это сделать:

```elixir
# Сделать первую букву строки заглавной в Elixir
string = "elixir programming"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string

# Вывод будет:
# Elixir programming
```

```elixir
# Сделать первую букву каждого слова в строке заглавной
string = "elixir programming language"
capitalized_words = String.split(string)
                    |> Enum.map(&String.capitalize/1)
                    |> Enum.join(" ")

IO.puts capitalized_words

# Вывод будет:
# Elixir Programming Language
```

## Подробнее

В далёкие времена компьютерного программирования языки программирования часто не заботились о манипуляциях со строками как части ядра языка. Однако, Elixir поставляется с мощным модулем функций для работы со строками "из коробки", благодаря своим корням в зрелой Erlang VM (BEAM). Сделать строку с заглавной буквы в Elixir очень просто с помощью модуля `String`.

Помимо прямолинейной `String.capitalize/1`, вы можете столкнуться с ситуациями, требующими более сложного поведения. Предположим, вам нужно сделать заглавными буквы в названиях или именах, учитывая культурные особенности. Один только модуль `String` Elixir'а здесь не поможет; вам стоит обратиться к библиотекам вроде `Cldr` для поддержки интернационализации.

Внутри `String.capitalize/1` учитывает Unicode и мультибайтовые символы, не только ASCII. Это значит, что он корректно обрабатывает широкий спектр языков и алфавитов, а не только английский текст.

Как альтернативу, вы можете написать свою функцию для преобразования в заглавные буквы, но в большинстве случаев должно хватить встроенных методов. С собственной реализацией вы рискуете столкнуться с тонкими ошибками, особенно при работе с международными текстами. Зачем изобретать велосипед, если у вас уже есть качественные инструменты?

## Смотрите также

- Официальная документация по модулю `String` в Elixir: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Elixir School для изучения строк и других основ: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)
- Библиотека ExCldr для поддержки интернационализации: [https://hex.pm/packages/ex_cldr](https://hex.pm/packages/ex_cldr)
