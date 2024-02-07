---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:03:20.011950-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/bash/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Регулярные выражения (regex) — это шаблоны, определяющие критерии поиска текста. Программисты используют их для поиска соответствий, замены или извлечения фрагментов из строк на основе этих шаблонов — подумайте о сложном поиске и замене на стероидах.

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
