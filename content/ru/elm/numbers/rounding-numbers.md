---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:03.858717-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u041C\u043E\u0434\u0443\u043B\u044C `Basics` \u0432 Elm \u043F\u0440\
  \u0435\u0434\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442 \u0443\u0434\u043E\
  \u0431\u043D\u044B\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u0438 \u0434\u043B\
  \u044F \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u044F: `round`, `floor`\
  \ \u0438 `ceiling`. \u0412\u043E\u0442 \u043A\u0430\u043A \u0438\u043C\u0438 \u043F\
  \u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C\u0441\u044F."
lastmod: '2024-03-13T22:44:44.891824-06:00'
model: gpt-4-0125-preview
summary: "\u041C\u043E\u0434\u0443\u043B\u044C `Basics` \u0432 Elm \u043F\u0440\u0435\
  \u0434\u043E\u0441\u0442\u0430\u0432\u043B\u044F\u0435\u0442 \u0443\u0434\u043E\u0431\
  \u043D\u044B\u0435 \u0444\u0443\u043D\u043A\u0446\u0438\u0438 \u0434\u043B\u044F\
  \ \u043E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u044F."
title: "\u041E\u043A\u0440\u0443\u0433\u043B\u0435\u043D\u0438\u0435 \u0447\u0438\u0441\
  \u0435\u043B"
weight: 13
---

## Как это сделать:
Модуль `Basics` в Elm предоставляет удобные функции для округления: `round`, `floor` и `ceiling`. Вот как ими пользоваться.

```elm
import Basics exposing (round, floor, ceiling)

-- Округлить до ближайшего целого числа
round 3.14    --> 3
round 3.5     --> 4

-- Округлить вниз
floor 3.999   --> 3

-- Округлить вверх
ceiling 3.001 --> 4

-- Отбросить десятичные знаки без округления
truncate 3.76 --> 3
```

Elm также предоставляет `toLocaleString` для округления до фиксированного числа десятичных знаков:

```elm
import Float exposing (toLocaleString)

-- Округлить до двух десятичных знаков
toLocaleString 2 3.14159 --> "3.14"
```

## Подробнее
Elm - это строго типизированный функциональный язык, который ограничивает побочные эффекты "краями" архитектуры. Это означает, что функции вроде округления должны быть чистыми и предсказуемыми. Исторически округление является обычной операцией во многих языках программирования, что связано с неточностью арифметики с плавающей точкой.

Подход Elm к округлению прост и логичен - функции чисты и соответствуют математическим определениям для round, floor и ceiling. Elm предвидит общие потребности, предоставляя встроенные функции, так как управление точностью часто требуется, особенно в финансах и графике.

К альтернативам встроенным функциям Elm могли бы относиться пользовательские реализации с использованием арифметических операций, но это добавляет ненужную сложность, когда стандартная библиотека уже эффективно справляется с задачей.

На данный момент Elm использует под капотом арифметику с плавающей точкой JavaScript для этих операций, следовательно, оставаясь в соответствии со стандартом IEEE 754. Это важно помнить, учитывая точность и потенциальные ошибки с плавающей точкой.

## См. также
- Официальная документация модуля `Basics` Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Подробный обзор работы чисел с плавающей точкой в вычислениях: https://floating-point-gui.de/
- Модуль `Float` Elm для дополнительных операций с числами с плавающей точкой: https://package.elm-lang.org/packages/elm/core/latest/Float
