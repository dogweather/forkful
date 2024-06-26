---
date: 2024-01-26 04:40:05.993993-07:00
description: "\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438: Elm \u043D\u0435\
  \ \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u043E\u0457\
  \ \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438 \u043A\u043E\u043C\u043F\
  \u043B\u0435\u043A\u0441\u043D\u0438\u0445 \u0447\u0438\u0441\u0435\u043B, \u0442\
  \u043E\u043C\u0443 \u0432\u0430\u043C \u043F\u043E\u0442\u0440\u0456\u0431\u043D\
  \u043E \u0441\u0442\u0432\u043E\u0440\u0438\u0442\u0438 \u0432\u043B\u0430\u0441\
  \u043D\u0438\u0439 \u0442\u0438\u043F \u0442\u0430 \u0444\u0443\u043D\u043A\u0446\
  \u0456\u0457. \u041E\u0441\u044C \u0448\u0432\u0438\u0434\u043A\u0435 \u043D\u0430\
  \u043B\u0430\u0448\u0442\u0443\u0432\u0430\u043D\u043D\u044F."
lastmod: '2024-03-13T22:44:49.140610-06:00'
model: gpt-4-0125-preview
summary: "Elm \u043D\u0435 \u043C\u0430\u0454 \u0432\u0431\u0443\u0434\u043E\u0432\
  \u0430\u043D\u043E\u0457 \u043F\u0456\u0434\u0442\u0440\u0438\u043C\u043A\u0438\
  \ \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u0438\u0445 \u0447\u0438\
  \u0441\u0435\u043B, \u0442\u043E\u043C\u0443 \u0432\u0430\u043C \u043F\u043E\u0442\
  \u0440\u0456\u0431\u043D\u043E \u0441\u0442\u0432\u043E\u0440\u0438\u0442\u0438\
  \ \u0432\u043B\u0430\u0441\u043D\u0438\u0439 \u0442\u0438\u043F \u0442\u0430 \u0444\
  \u0443\u043D\u043A\u0446\u0456\u0457."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u0438\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

## Як робити:
Elm не має вбудованої підтримки комплексних чисел, тому вам потрібно створити власний тип та функції. Ось швидке налаштування:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Приклад використання:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum є { real = 4.0, imaginary = -2.0 }
```

## Поглиблене занурення
Історично, комплексні числа не завжди приймались. Вони стали переломним моментом у 16-му столітті для вирішення кубічних рівнянь. Альтернативи в інших мовах, як-от Python, пропонують вбудовану підтримку комплексних чисел з операціями "відразу з коробки". Elm вимагає підходу «зроби сам», як ви вже бачили. Але ви можете зробити його настільки складним, наскільки потрібно, побудувавши множення, ділення та інші операції, налаштувавши питання продуктивності.

## Також дивіться
- Офіційна документація Elm: https://package.elm-lang.org/ для створення власних типів та освоєння основ Elm.
- Любителі історії математики могли б переглянути "An Imaginary Tale" Пола Дж. Нахіна для подорожі комплексних чисел крізь час.
- Зануритись у математично орієнтовані програмувальні виклики на Project Euler (https://projecteuler.net), щоб застосувати ваші знання комплексних чисел.
