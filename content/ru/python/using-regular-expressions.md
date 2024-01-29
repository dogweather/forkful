---
title:                "Использование регулярных выражений"
date:                  2024-01-29T00:03:52.921851-07:00
model:                 gpt-4-0125-preview
simple_title:         "Использование регулярных выражений"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/using-regular-expressions.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Регулярные выражения (regex) — это шаблоны, используемые для поиска комбинаций символов в строках. Программисты используют regex для поиска, редактирования или манипулирования текстом, поскольку это мощный и эффективный инструмент.

## Как использовать:
Ниже приведены примеры на Python с использованием модуля `re` для общих операций с regex:

```Python
import re

# Найти все совпадения 'abc' в строке
matches = re.findall('abc', 'abc123abc')
print(matches)  # Результат: ['abc', 'abc']

# Искать 'def' и возвращать объект Match
match = re.search('def', '123def456')
if match:
    print(match.group())  # Результат: 'def'

# Заменить 'ghi' на 'xyz'
replaced = re.sub('ghi', 'xyz', 'ghi123ghi')
print(replaced)  # Результат: 'xyz123xyz'
```

## Глубже в тему
Регулярные выражения существуют с 1950-х годов, развиваясь параллельно с теорией формальных языков. В качестве альтернативы regex можно использовать библиотеки для разбора и строковые методы вроде `str.find()` или `str.replace()`, однако они не обладают такой универсальностью поиска шаблонов, как regex. С точки зрения реализации, Python использует модуль `re`, который основан на традиционной UNIX-библиотеке regex, но включает в себя некоторые улучшения.

## Смотрите также
- Документация модуля Python `re`: https://docs.python.org/3/library/re.html
- Руководство по синтаксису регулярных выражений: https://www.regular-expressions.info/
- Тестер и отладчик regex: https://regex101.com/
