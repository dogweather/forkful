---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:38.465477-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: \u0412 Fish Shell \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u0430 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0430 \u0440\u0435\u0433\
  \u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\u0430\u0436\u0435\u043D\
  \u0438\u0439 \u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u0430\u0445, \u0442\u0430\
  \u043A\u0438\u0445 \u043A\u0430\u043A `string`. \u0414\u0430\u0432\u0430\u0439\u0442\
  \u0435 \u0440\u0430\u0441\u0441\u043C\u043E\u0442\u0440\u0438\u043C \u043D\u0435\
  \u043A\u043E\u0442\u043E\u0440\u044B\u0435 \u043F\u0440\u0438\u043C\u0435\u0440\u044B\
  : **\u0411\u0430\u0437\u043E\u0432\u044B\u0439\u2026"
lastmod: '2024-03-13T22:44:45.817990-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Fish Shell \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u0430 \u043F\
  \u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0430 \u0440\u0435\u0433\u0443\u043B\u044F\
  \u0440\u043D\u044B\u0445 \u0432\u044B\u0440\u0430\u0436\u0435\u043D\u0438\u0439\
  \ \u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u0430\u0445, \u0442\u0430\u043A\u0438\
  \u0445 \u043A\u0430\u043A `string`."
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

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
