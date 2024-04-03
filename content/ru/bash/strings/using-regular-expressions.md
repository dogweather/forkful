---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:20.011950-07:00
description: "\u041A\u0430\u043A \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\
  \u0430\u0442\u044C: ."
lastmod: '2024-03-13T22:44:45.347153-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u043D\u0438\u0435\
  \ \u0440\u0435\u0433\u0443\u043B\u044F\u0440\u043D\u044B\u0445 \u0432\u044B\u0440\
  \u0430\u0436\u0435\u043D\u0438\u0439"
weight: 11
---

## Как использовать:
```Bash
# Поиск по шаблону
echo "I love to code in Bash" | grep -oP 'code'

# Вывод:
code

# Замена строки с использованием regex и sed
echo "Bash 2023" | sed -E 's/[0-9]+/2024/'

# Вывод:
Bash 2024

# Извлечение подстроки с помощью regex
echo "Error: Line 42" | grep -oP '(?<=Line )\d+'

# Вывод:
42
```

## Глубокое погружение
Регулярные выражения существуют с 1950-х годов, впервые были предложены математиком Стивеном Клини. Альтернативы regex в Bash включают использование `awk` или `perl`, которые имеют собственные возможности regex. С точки зрения реализации, Bash использует grep для поиска соответствий, `sed` для поиска и замены, и оператор `=~` внутри `[[ ]]` для условных выражений. Имейте в виду, что regex может различаться между инструментами (`grep`, `egrep`, `sed` и `awk`), так что знайте, с каким "вкусом" вы работаете.

## Смотрите также
- [Руководство по GNU Grep](https://www.gnu.org/software/grep/manual/grep.html)
- [Sed - Введение и учебник](https://www.grymoire.com/Unix/Sed.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [Regex101: Онлайн тестер и отладчик regex](https://regex101.com/)
