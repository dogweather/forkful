---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:00:13.984893-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0414\u043E\u043F\u0443\u0441\u0442\u0438\u043C, \u0432\u044B \u043F\
  \u0438\u0448\u0435\u0442\u0435 \u0441\u043A\u0440\u0438\u043F\u0442 \u0434\u043B\
  \u044F \u0440\u0430\u0441\u0447\u0435\u0442\u0430 \u043A\u0432\u0430\u0434\u0440\
  \u0430\u0442\u0430 \u0438 \u043A\u0443\u0431\u0430 \u0447\u0438\u0441\u043B\u0430\
  . \u0411\u0435\u0437 \u0444\u0443\u043D\u043A\u0446\u0438\u0439 \u043F\u043E\u043B\
  \u0443\u0447\u0430\u0435\u0442\u0441\u044F \u043A\u0430\u0448\u0430 \u0438\u0437\
  \ \u043F\u043E\u0432\u0442\u043E\u0440\u0435\u043D\u0438\u0439."
lastmod: '2024-03-13T22:44:44.280409-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043E\u043F\u0443\u0441\u0442\u0438\u043C, \u0432\u044B \u043F\u0438\
  \u0448\u0435\u0442\u0435 \u0441\u043A\u0440\u0438\u043F\u0442 \u0434\u043B\u044F\
  \ \u0440\u0430\u0441\u0447\u0435\u0442\u0430 \u043A\u0432\u0430\u0434\u0440\u0430\
  \u0442\u0430 \u0438 \u043A\u0443\u0431\u0430 \u0447\u0438\u0441\u043B\u0430."
title: "\u041E\u0440\u0433\u0430\u043D\u0438\u0437\u0430\u0446\u0438\u044F \u043A\u043E\
  \u0434\u0430 \u0432 \u0444\u0443\u043D\u043A\u0446\u0438\u0438"
weight: 18
---

## Как это сделать:
Допустим, вы пишете скрипт для расчета квадрата и куба числа. Без функций получается каша из повторений:

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
Вывод:
```
Квадрат: 16, Куб: 64
Квадрат: 25, Куб: 125
```

С функциями выглядит аккуратнее:

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
Вывод:
```
Квадрат: 16, Куб: 64
Квадрат: 25, Куб: 125
```

## Погружение
В стародавние времена, когда программы были простыми, можно было обойтись простым списком инструкций. Но по мере усложнения программного обеспечения, разработчики поняли, что они переписывают один и тот же код снова и снова. Здравствуйте, функции — переиспользуемые блоки кода, выполняющие одно действие.

Альтернативы функциям включают классы (объединение функций с данными, на которых они оперируют) и встраиваемый код (интеллект там, где он нужен, но рискованный для сложных задач). С точки зрения реализации, хитрость не только в создании функций, но и в их способности хорошо выполнять одну задачу — подумайте о принципе единственной ответственности. Функции также идеально должны быть без состояния, что означает отсутствие сюрпризов с данными, поступающими внутрь или выходящими наружу.

## Смотрите также
- Официальные руководства Python по функциям: https://docs.python.org/3/tutorial/controlflow.html#defining-functions
- "Чистый код" Роберта Мартина, принципы написания аккуратных функций.
- "Рефакторинг: Улучшение проекта существующего кода" Мартина Фаулера, который включает примеры организации кода.
