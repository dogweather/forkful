---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:38.734951-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u041F\u0440\u0438\u0432\u0435\u0434\u0435\u043D\u0438\u0435 \u0441\
  \u0442\u0440\u043E\u043A\u0438 \u043A \u043D\u0438\u0436\u043D\u0435\u043C\u0443\
  \ \u0440\u0435\u0433\u0438\u0441\u0442\u0440\u0443 \u0432 Python \u043F\u0440\u043E\
  \u0441\u0442\u043E \u0441 \u043C\u0435\u0442\u043E\u0434\u043E\u043C `.lower()`."
lastmod: '2024-03-13T22:44:44.246645-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u0440\u0438\u0432\u0435\u0434\u0435\u043D\u0438\u0435 \u0441\u0442\
  \u0440\u043E\u043A\u0438 \u043A \u043D\u0438\u0436\u043D\u0435\u043C\u0443 \u0440\
  \u0435\u0433\u0438\u0441\u0442\u0440\u0443 \u0432 Python \u043F\u0440\u043E\u0441\
  \u0442\u043E \u0441 \u043C\u0435\u0442\u043E\u0434\u043E\u043C `.lower()`."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0438\
  \u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 4
---

## Как это сделать:
Приведение строки к нижнему регистру в Python просто с методом `.lower()`.
```Python
original_string = "Hello, World!"
lowercase_string = original_string.lower()
print(lowercase_string)  # Вывод: hello, world!
```
Или используйте списочное включение для большего контроля:
```Python
s = "HELLO, World!"
lower_list = [char.lower() for char in s]
print(''.join(lower_list))  # Вывод: hello, world!
```

## Углубленный анализ
Метод `.lower()` был частью типа строк в Python довольно давно. Это простой способ обеспечить обработку данных без учета регистра, что полезно в ситуациях, таких как регистронезависимый ввод пользователя.

Есть альтернативы, например, использование регулярных выражений:
```Python
import re

s = "HELLO, World!"
lower_s = re.sub(r'[A-Z]', lambda match: match.group(0).lower(), s)
print(lower_s)  # Вывод: hello, world!
```
Но это избыточно просто для преобразования строки в нижний регистр.

Внутри, метод `.lower()` Python полагается на карту символов Unicode. Стандарт Unicode определяет эквивалент в нижнем регистре почти всех символов, имеющих регистр. Этот процесс более сложный, чем просто вычитание значения для перехода от 'A' к 'a', поскольку не все языки и системы письма имеют такое простое и прямое сопоставление.

## Смотри также
- Документация Python по методам строк: https://docs.python.org/3/library/stdtypes.html#string-methods
- Подробности маппинга регистра в Unicode: https://www.unicode.org/reports/tr21/tr21-5.html
- Учебник по списочным включениям в Python: https://realpython.com/list-comprehension-python/
