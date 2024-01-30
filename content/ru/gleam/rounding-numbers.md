---
title:                "Округление чисел"
date:                  2024-01-29T00:02:08.954463-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округление чисел"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/rounding-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Округление чисел предполагает корректировку значения к ближайшему указанному разряду — например, с 2.56 до 3, если мы округляем до целых чисел. Программисты делают это для упрощения или чтобы соответствовать определенным числовым спецификациям, обычно чтобы избежать нюансов, вызванных ошибками точности с плавающей точкой, или чтобы сделать вывод понятным для пользователя.

## Как это сделать:
В Gleam округление на момент моей последней проверки не входит в стандартную библиотеку, но вот как вы обычно округляете число с плавающей точкой до ближайшего целого, используя функции Erlang напрямую:

```gleam
external fn erlang_round(Float) -> Int = "erlang" "round"

pub fn main() {
  let rounded = erlang_round(2.56)
  rounded // Вывод: 3
}
```

Вывод:
```
3
```

Имеете в виду другую точность? Например, округление до двух десятичных знаков? Нам понадобится немного математики:

```gleam
pub fn round_to_two_places(num: Float) -> Float {
  let multiplier = 100.0
  let tmp = num * multiplier
  let round_tmp = erlang_round(tmp)
  round_tmp / multiplier
}

pub fn main() {
    round_to_two_places(2.569) // Вывод: 2.57
}
```

Вывод:
```
2.57
```

## Подробнее
Исторически округление чисел было критически важным, особенно в финансовых и научных вычислениях, где точность и стандарты очень важны. Без округления вы бы получили длинные десятичные дроби повсюду, что делает вычисления непрактичными и склонными к ошибкам.

В мире программирования разные языки предлагают разные подходы, от встроенных функций до обширных математических библиотек. Округление может включать разные правила — например, "округление до ближайшего большего" (обычный метод) или "округление до ближайшего четного" (часто используется в финансовых расчетах, чтобы избежать предвзятости).

Gleam, будучи молодым языком с корнями в Erlang, опирается на мощный набор числовых функций Erlang. По мере развития языка мы можем увидеть введение собственных функций, уменьшающее необходимость вызывать внешние процедуры.

## Смотрите также
- Модуль :math Erlang для дополнительных вычислений с числами: https://erlang.org/doc/man/math.html
- Для понимания, почему округление может быть сложным, стандарт IEEE с плавающей точкой: https://ieeexplore.ieee.org/document/8766229
- Интересуетесь математикой за этим? Почитайте "Что каждый компьютерный учёный должен знать о арифметике с плавающей точкой": https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html