---
date: 2024-01-26 01:11:48.627052-07:00
description: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\
  \u043E\u0434\u0443 \u0443 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u043F\u043E\
  \u043B\u044F\u0433\u0430\u0454 \u0443 \u0440\u043E\u0437\u0431\u0438\u0442\u0442\
  \u0456 \u0432\u0430\u0448\u043E\u0433\u043E \u043A\u043E\u0434\u0443 \u043D\u0430\
  \ \u043F\u043E\u0432\u0442\u043E\u0440\u043D\u043E \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u043D\u0456 \u0431\u043B\u043E\u043A\
  \u0438 \u0437 \u043A\u043E\u043D\u043A\u0440\u0435\u0442\u043D\u0438\u043C\u0438\
  \ \u0446\u0456\u043B\u044F\u043C\u0438. \u041C\u0438 \u0440\u043E\u0431\u0438\u043C\
  \u043E \u0446\u0435, \u0449\u043E\u0431 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  \ \u043A\u043E\u0434 \u0447\u0438\u0441\u0442\u0456\u0448\u0438\u043C,\u2026"
lastmod: '2024-03-13T22:44:48.592603-06:00'
model: gpt-4-1106-preview
summary: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\
  \u043E\u0434\u0443 \u0443 \u0444\u0443\u043D\u043A\u0446\u0456\u0457 \u043F\u043E\
  \u043B\u044F\u0433\u0430\u0454 \u0443 \u0440\u043E\u0437\u0431\u0438\u0442\u0442\
  \u0456 \u0432\u0430\u0448\u043E\u0433\u043E \u043A\u043E\u0434\u0443 \u043D\u0430\
  \ \u043F\u043E\u0432\u0442\u043E\u0440\u043D\u043E \u0432\u0438\u043A\u043E\u0440\
  \u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u043D\u0456 \u0431\u043B\u043E\u043A\
  \u0438 \u0437 \u043A\u043E\u043D\u043A\u0440\u0435\u0442\u043D\u0438\u043C\u0438\
  \ \u0446\u0456\u043B\u044F\u043C\u0438. \u041C\u0438 \u0440\u043E\u0431\u0438\u043C\
  \u043E \u0446\u0435, \u0449\u043E\u0431 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  \ \u043A\u043E\u0434 \u0447\u0438\u0441\u0442\u0456\u0448\u0438\u043C,\u2026"
title: "\u041E\u0440\u0433\u0430\u043D\u0456\u0437\u0430\u0446\u0456\u044F \u043A\u043E\
  \u0434\u0443 \u0432 \u0444\u0443\u043D\u043A\u0446\u0456\u0457"
---

{{< edit_this_page >}}

## Що і чому?
Організація коду у функції полягає у розбитті вашого коду на повторно використовувані блоки з конкретними цілями. Ми робимо це, щоб зробити код чистішим, легшим для читання, відлагодження та оновлення.

## Як це зробити:
Скажімо, ви пишете скрипт для обчислення квадрата та куба числа. Без функцій це безладний набір повторень:

```Python
num = 4
square = num * num
cube = num * num * num
print(f"Квадрат: {square}, Куб: {cube}")

num = 5
square = num * num
cube = num * num * num
print(f"Квадрат: {square}, Куб: {cube}")
```
Вивід:
```
Квадрат: 16, Куб: 64
Квадрат: 25, Куб: 125
```

З функціями це виглядає акуратніше:

```Python
def square(n):
    return n * n

def cube(n):
    return n ** 3

num = 4
print(f"Квадрат: {square(num)}, Куб: {cube(num)}")

num = 5
print(f"Квадрат: {square(num)}, Куб: {cube(num)}")
```
Вивід:
```
Квадрат: 16, Куб: 64
Квадрат: 25, Куб: 125
```

## Поглиблений огляд
У давні часи, коли програми були простими, можна було обійтися просто списком інструкцій. Але коли програмне забезпечення стало складнішим, розробники зрозуміли, що вони переписують один і той же код знову і знову. Вітаємо, функції — повторно використовувані блоки коду, які виконують одну дію.

Альтернативи функціям включають класи (об’єднання функцій з даними, на які вони впливають) та вбудований код (інтелект там, де вам це потрібно, але ризиковано для складних завдань). У плані реалізації хитрість полягає не просто у створенні функцій, а в тому, щоб вони добре робили одну річ - подумайте про принцип єдиної відповідальності. Функції також, як ідеал, повинні бути безстанові, що означає відсутність сюрпризів з даними, що надходять або виходять.

## Дивіться також
- Офіційні пітонівські туторіали з функцій: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- 'Чистий код' Роберта С. Мартіна, для принципів написання чистих функцій.
- 'Рефакторинг: Удосконалення дизайну існуючого коду' Мартіна Фаулера, що включає приклади організації коду.
