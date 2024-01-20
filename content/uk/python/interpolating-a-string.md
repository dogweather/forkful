---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо?

Інтерполяція рядків - це процес вставки значень змінних прямо в рядки. Програмісти це роблять, щоб створити більш зручний і структурований код, ніж при конкатенації рядків.

## Як це робиться:
Ось базовий приклад інтерполяції рядків у Python.
```Python
name = "Ivan"
age = 30
print(f"Мене звати {name} і мені {age} років.")
```
Цей код виведе: "Мене звати Ivan і мені 30 років."

## Поглиблений аналіз
Історично, Python різними способами дозволяв зробити інтерполяцію рядків. Методи `%s` та `.format()` були популярними у минулому, але зі вступом Python 3.6, f-strings стали більш ефективними та читаємими.

Ось приклад використання `%s` та `.format()`:
```Python
name = "Ivan"
age = 30

# з використанням %s
print("Мене звати %s і мені %s років." % (name, age)) 


# з використанням .format()
print("Мене звати {} і мені {} років.".format(name, age)) 
```
Це приведе до того ж самого виводу, але як бачите, f-string є набагато чистішим і простішим для читання.

## Дивіться також
Ці посилання допоможуть вам дізнатися більше про інтерполяцію рядків в Python:

1. [Документація Python про f-рядки](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)
2. [Порівняння інтерполяції рядків у Python](https://realpython.com/python-f-strings/)