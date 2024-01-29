---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:03:38.465477-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/fish-shell/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Регулярные выражения, или regex, — это шаблоны, описывающие наборы строк. Программисты используют их для поиска, сопоставления и манипуляций с текстом — очень удобно для нахождения иголок в стогах данных.

## Как использовать:
В Fish Shell встроена поддержка регулярных выражений в командах, таких как `string`. Давайте рассмотрим некоторые примеры:

**Базовый поиск:**

Найти наличие слова "fish" в строке:

```fish
echo "I love to fish for fish in my fish tank" | string match -r "fish"
```

Вывод:

```
fish
fish
fish
```

**Группы захвата:**

Извлечь соответствующие группы с помощью круглых скобок:

```fish
echo "Color: Blue, Code: #0000FF" | string match -r "Color: (\w+)"
```

Вывод:

```
Color: Blue
Blue
```

**Замена текста:**

Заменить "fish" на "shark":

```fish
echo "One fish, two fish, red fish, blue fish" | string replace -ar "fish" "shark"
```

Вывод:

```
One shark, two shark, red shark, blue shark
```

## Глубокое погружение:
Регулярные выражения происходят из теоретической информатики, их придумали в 1950-х годах. Альтернативы? Конечно, есть простой поиск по строкам или парсеры для большей структурированности, но regex удобен для быстрых и "грязных" задач. Fish Shell использует PCRE (Perl Compatible Regular Expressions) под капотом, что обеспечивает надежный набор функций для сопоставления шаблонов.

## Смотрите также:
- Официальная документация Fish Shell: [Команда string](https://fishshell.com/docs/current/cmds/string.html)
- Учебник по регулярным выражениям для начинающих: [Regular Expressions 101](https://regex101.com/)
- Глубокое понимание: [Mastering Regular Expressions, автор Jeffrey Friedl](http://shop.oreilly.com/product/9780596528126.do)
