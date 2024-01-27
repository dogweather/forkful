---
title:                "Округлення чисел"
date:                  2024-01-26T03:45:09.204797-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округлення чисел"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/rounding-numbers.md"
---

{{< edit_this_page >}}

## Що та Чому?

Округлення чисел полягає в коригуванні десяткового числа до найближчого цілого значення або до вказаної кількості десяткових знаків. Програмісти вдаються до округлення для зменшення складності, покращення читабельності або відповідності вимогам точності.

## Як:

Модуль `Basics` в Elm надає зручні функції для округлення: `round`, `floor` та `ceiling`. Ось як їх використовувати.

```elm
import Basics exposing (round, floor, ceiling)

-- Округлення до найближчого цілого числа
round 3.14    --> 3
round 3.5     --> 4

-- Округлення вниз
floor 3.999   --> 3

-- Округлення вгору
ceiling 3.001 --> 4

-- Відкидання десяткових знаків без округлення
truncate 3.76 --> 3
```

Elm також надає `toLocaleString` для округлення до фіксованої кількості десяткових знаків:

```elm
import Float exposing (toLocaleString)

-- Округлення до двох десяткових знаків
toLocaleString 2 3.14159 --> "3.14"
```

## Поглиблений огляд

Elm — це мова програмування з сильною типізацією, яка обмежує побічні ефекти до "країв" архітектури. Це означає, що функції на кшталт округлення мають бути чистими та передбачуваними. Історично округлення є поширеною операцією в багатьох мовах програмування, які мають справу з неточністю чисел з плаваючою комою.

Підхід Elm до округлення є простим - функції є чистими та відповідають математичним визначенням округлення, округлення вниз та округлення вгору. Elm передбачає поширені потреби, надаючи вбудовані функції, оскільки управління точністю часто є вимогою, особливо в фінансах та графіці.

Альтернативи до вбудованих функцій Elm можуть включати власні імплементації за допомогою арифметичних операцій, але це додає непотрібну складність, коли стандартна бібліотека вже ефективно виконує роботу.

Наразі Elm використовує підлоговий математичний модуль з плаваючою комою від JavaScript для цих операцій, тим самим залишаючись узгодженим із стандартом IEEE 754, що варто пам'ятати, коли йдеться про точність та потенційні помилки з числами з плаваючою комою.

## Дивіться також

- Офіційна документація модуля `Basics` в Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Детальний огляд того, як числа з плаваючою комою працюють у комп'ютерах: https://floating-point-gui.de/
- Модуль `Float` в Elm для більшої кількості операцій з числами з плаваючою комою: https://package.elm-lang.org/packages/elm/core/latest/Float