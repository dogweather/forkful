---
title:                "Преобразование строки в нижний регистр"
aliases: - /ru/python/converting-a-string-to-lower-case.md
date:                  2024-01-28T23:56:38.734951-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Преобразование строки в нижний регистр подразумевает изменение всех символов верхнего регистра в тексте на их эквиваленты в нижнем регистре. Программисты часто делают это для обеспечения консистентности, сравнения или поиска, поскольку 'A' не то же самое, что и 'a' в мире компьютеров.

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
