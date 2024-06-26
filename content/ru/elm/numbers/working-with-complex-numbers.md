---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:14.854723-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Elm \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438\
  \ \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0445 \u0447\u0438\
  \u0441\u0435\u043B, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u0432\u044B \u0441\
  \u043E\u0437\u0434\u0430\u0434\u0438\u0442\u0435 \u0441\u043E\u0431\u0441\u0442\u0432\
  \u0435\u043D\u043D\u044B\u0439 \u0442\u0438\u043F \u0438 \u0444\u0443\u043D\u043A\
  \u0446\u0438\u0438. \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u0430\u044F\
  \ \u043D\u0430\u0441\u0442\u0440\u043E\u0439\u043A\u0430."
lastmod: '2024-03-13T22:44:44.890014-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elm \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\u043D\
  \u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438 \u043A\
  \u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\
  \u043B, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u0432\u044B \u0441\u043E\u0437\
  \u0434\u0430\u0434\u0438\u0442\u0435 \u0441\u043E\u0431\u0441\u0442\u0432\u0435\u043D\
  \u043D\u044B\u0439 \u0442\u0438\u043F \u0438 \u0444\u0443\u043D\u043A\u0446\u0438\
  \u0438."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u044B\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

## Как это сделать:
В Elm нет встроенной поддержки комплексных чисел, поэтому вы создадите собственный тип и функции. Вот быстрая настройка:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Пример использования:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum это { real = 4.0, imaginary = -2.0 }
```

## Глубокое погружение
Исторически комплексные числа не всегда принимались. Они стали изменить игру в 16-м веке для решения кубических уравнений. Альтернативы в других языках, например в Python, предлагают встроенную поддержку комплексных чисел с операциями прямо из коробки. В Elm требуется подход своими руками, как вы уже видели. Но вы можете сделать его настолько сложным, насколько это необходимо, создавая умножение, деление и другие операции, настраивая производительность.

## Смотрите также
- Официальная документация Elm: https://package.elm-lang.org/ для создания пользовательских типов и освоения основ Elm.
- Любителям истории математики могут проверить "An Imaginary Tale" от Paul J. Nahin для путешествия комплексных чисел сквозь время.
- Погрузитесь в математически ориентированные программные задачи на Project Euler (https://projecteuler.net) чтобы применить вашу магию комплексных чисел.
