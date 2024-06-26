---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:24:16.516821-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Python 3.6 \u0456 \u0432\u0438\u0449\u0435 \u0432\u0438 \u043C\u043E\u0436\
  \u0435\u0442\u0435 \u0456\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044E\u0432\u0430\
  \u0442\u0438 \u0440\u044F\u0434\u043A\u0438, \u0432\u0438\u043A\u043E\u0440\u0438\
  \u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 f-\u0440\u044F\u0434\u043A\u0438\
  . \u041E\u0441\u044C \u044F\u043A."
lastmod: '2024-03-13T22:44:48.560423-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Python 3.6 \u0456 \u0432\u0438\u0449\u0435 \u0432\u0438 \u043C\u043E\
  \u0436\u0435\u0442\u0435 \u0456\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044E\u0432\
  \u0430\u0442\u0438 \u0440\u044F\u0434\u043A\u0438, \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u044E\u0447\u0438 f-\u0440\u044F\u0434\u043A\
  \u0438."
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0430"
weight: 8
---

## Як це зробити:
У Python 3.6 і вище ви можете інтерполювати рядки, використовуючи f-рядки. Ось як:

```Python
name = 'Аліса'
age = 30
greeting = f"Привіт, {name}. Тобі {age} років."

print(greeting)
```

Вивід:
```
Привіт, Аліса. Тобі 30 років.
```

Ви також можете використовувати вирази всередині фігурних дужок:

```Python
a = 5
b = 10
info = f"П'ять плюс десять дорівнює {a + b}, а не {2 * (a + b)}."

print(info)
```

Вивід:
```
П'ять плюс десять дорівнює 15, а не 30.
```

## Поглиблено
До Python 3.6, `.format()` був способом інтерполяції рядків:

```Python
name = 'Боб'
age = 25
greeting = "Привіт, {}. Тобі {} років.".format(name, age)

print(greeting)
```

Старий добрий Python (версії < 2.6) використовував оператор `%` для інтерполяції, що є менш інтуїтивним і може стати заплутаним при використанні кількох змінних:

```Python
name = 'Керол'
age = 35
greeting = "Привіт, %s. Тобі %d років." % (name, age)

print(greeting)
```

Крім чистішого синтаксису, f-рядки швидші, оскільки вони обчислюються під час виконання та потім перетворюються безпосередньо на ефективну операцію форматування рядка. Метод `.format()` і оператор `%` вимагають більше кроків і працюють повільніше.

## Дивіться також
- [PEP 498 – Літеральна інтерполяція рядків](https://www.python.org/dev/peps/pep-0498/) для офіційної документації по f-рядках.
- [Python f-рядки](https://realpython.com/python-f-strings/) на Real Python для навчального посібника з використання f-рядків.
- [Метод .format()](https://docs.python.org/3/library/stdtypes.html#str.format) у документації Python для розуміння старішого методу форматування рядків `.format()`.
