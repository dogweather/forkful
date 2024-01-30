---
title:                "Поиск длины строки"
date:                  2024-01-28T23:58:09.371286-07:00
model:                 gpt-4-0125-preview
simple_title:         "Поиск длины строки"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/python/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Нахождение длины строки означает подсчет ее символов. Программисты делают это для валидации ввода данных, прохода по строкам, выделения ресурсов среди прочего.

## Как это сделать:

```python
# Простое использование функции len()
my_string = "Hello, World!"
length = len(my_string)
print(length)  # Вывод: 13

# Длина в цикле
for i in range(len(my_string)):
    print(my_string[i], end='')  # Вывод: Hello, World!
print()  # Для новой строки

# Сочетание длины строки с другими операциями
if len(my_string) > 10:
    print("Это длинная строка!")  # Вывод: Это длинная строка!
```

## Глубокое погружение

Исторически функция `len()` была основным способом Python'а для нахождения длины строки. Это элегантно и быстро. Внутри строки Python являются массивами байт, представляющими символы Unicode, и функция `len()` считает их. Функция работает не только со строками, но и с любым итерируемым объектом.

Альтернативы? Хотя для строк они и не часто используются, но вы могли бы проходиться по строке и вручную считать символы — это неуклюже и неэффективно. До поддержки Unicode длина строки иногда отличалась от ее размера в памяти, но поскольку с Python 3 строки являются нативно Unicode, `len()` точно отражает количество символов.

С точки зрения реализации, строки Python являются объектами с метаданными, включая длину, поэтому `len()` — это операция с постоянным временем выполнения O(1) — это как щелкнуть пальцами и получить ответ.

## Смотрите также

- Документация Python по `len()`: https://docs.python.org/3/library/functions.html#len
- Unicode и кодировка строк в Python: https://docs.python.org/3/howto/unicode.html
- Временная сложность для встроенных типов Python: https://wiki.python.org/moin/TimeComplexity