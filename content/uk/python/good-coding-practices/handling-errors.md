---
date: 2024-01-26 00:59:40.939116-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: \u041F\
  \u0440\u0438\u043A\u043B\u0430\u0434 \u0432\u0438\u0432\u043E\u0434\u0443 \u043F\
  \u0440\u0438 \u0432\u0432\u0435\u0434\u0435\u043D\u043D\u0456 \u043D\u0435\u043F\
  \u0440\u0430\u0432\u0438\u043B\u044C\u043D\u043E\u0433\u043E \u0447\u0438\u0441\u043B\
  \u0430 \u0432 \u043F\u0435\u0440\u0448\u043E\u043C\u0443 \u0431\u043B\u043E\u043A\
  \u0443."
lastmod: '2024-04-05T21:53:48.857810-06:00'
model: gpt-4-1106-preview
summary: "\u041F\u0440\u0438\u043A\u043B\u0430\u0434 \u0432\u0438\u0432\u043E\u0434\
  \u0443 \u043F\u0440\u0438 \u0432\u0432\u0435\u0434\u0435\u043D\u043D\u0456 \u043D\
  \u0435\u043F\u0440\u0430\u0432\u0438\u043B\u044C\u043D\u043E\u0433\u043E \u0447\u0438\
  \u0441\u043B\u0430 \u0432 \u043F\u0435\u0440\u0448\u043E\u043C\u0443 \u0431\u043B\
  \u043E\u043A\u0443."
title: "\u041E\u0431\u0440\u043E\u0431\u043A\u0430 \u043F\u043E\u043C\u0438\u043B\u043E\
  \u043A"
weight: 16
---

## Як це робити:
``` Python
# Базовий блок try-except
try:
    # ризикований код
    number = int(input("Введіть число: "))
except ValueError:
    # обробити помилку
    print("Це не число!")

# Вказівка декількох винятків
try:
    # код, який може викликати різні винятки
    result = 10 / int(input("Введіть дільник: "))
except ZeroDivisionError:
    print("Упс! Не можна ділити на нуль.")
except ValueError:
    print("Мені потрібне число, друже.")

# Використання else і finally
try:
    number = int(input("Введіть число для піднесення до квадрату: "))
except ValueError:
    print("Я сказав число!")
else:
    # помилок не виникло
    print("Квадрат вашого числа:", number**2)
finally:
    # завжди виконується
    print("Дякую, що спробували це!")
```

Приклад виводу при введенні неправильного числа в першому блоку:
```
Введіть число: привіт
Це не число!
```

## Підводимо підсумки
З початку програмування обробка помилок була критично важливою. Ранні підходи були примітивними, наприклад, перевірка умов перед кожною ризикованою операцією. Синтаксис `try-except` у Python запозичений від спадку обробки винятків у старших мовах, таких як C++ та Java, що спрощує процес.

Коли ви використовуєте `try` для блоку коду, Python спостерігає за будь-якими винятками. Якщо помилка виникає, блок `except` ловить її. Ви можете конкретизувати, які винятки ловити, або ловити всі за допомогою голого `except`. Однак, метод "конкретики на першому місці" є кращим підходом – він точний, а не просто всезагальний.

`else` та `finally` – це додаткові інструменти в цій концепції. Блок `else` виконується, якщо у блоку try не виникло помилок. `finally` – це надійний друг, що виконується незалежно від чого – думайте про операції очищення.

Альтернативи? Безумовно є. Деякі мови використовують коди повернення замість виключень. Ви можете також зіткнутися з операторами `with` для управління ресурсами або `assertions` для перевірки умов під час розробки. Але коли ми говоримо про міцні стратегії обробки помилок, модель try-catch виділяється завдяки своїй читабельності та структурі.

## Дивіться також
Ось деякі гарні додаткові ресурси для глибшого занурення в тему:

- Офіційна документація Python про помилки та винятки: [Python Docs – Помилки та винятки](https://docs.python.org/3/tutorial/errors.html)
- Посібник від Real Python на цю тему: [Real Python - Блок try/except/else/finally](https://realpython.com/python-exceptions/)
- Обговорення найкращих практик обробки помилок: [Stack Overflow – Як правильно ігнорувати винятки?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
