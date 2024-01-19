---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що й навіщо?
Знаходження довжини рядка в програмуванні означає визначення кількості символів у цьому рядку. Нам потрібно це робити для управління обробкою даних цього рядка - наприклад, при зчитуванні інформації з файлів, перевірці введення користувача або будь-якій іншій ситуації, де потрібно знати об'єм тексту.

## Як це робиться:
Ось базовий приклад коду Python, який показує, як знайти довжину рядка.

```Python
my_string = "Програмування на Python"
length = len(my_string)
print(length)
```

Виведення:

```Python
23
```

Функція `len()` в Python повертає довжину рядка.

## Глибше занурення:
i. Історичний контекст: Функція `len()` була частиною Python з самого початку, підтверджуючи важливість операцій з рядками в програмуванні.
ii. Альтернативи: Крім `len()`, можна використати цикл `for` для обрахунку довжини рядка:
```Python
count = 0
for letter in my_string:
    count += 1
print(count)
```
iii. Внутрішні особливості: функція `len()` в Python викликає метод об'єкта `__len__()`, який повертає довжину об'єкта.  Це пояснює, чому `len()` працює з різними типами даних, такими як рядки, списки, кортежі та інші.

## Дивіться також:
Чудові ресурси для вивчення Python і рядків:

- [Official Python Documentation](https://docs.python.org/3/tutorial/introduction.html#strings)
- [Real Python: Python Strings](https://realpython.com/python-strings/)
- [W3Schools: Python Strings](https://www.w3schools.com/python/python_strings.asp)