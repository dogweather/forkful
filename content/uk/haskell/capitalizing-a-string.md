---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"

category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це таке та Навіщо?
Перетворення рядка на великі літери означає зміну всіх маленьких літер на великі. Програмісти використовують це для нормалізації тексту для порівняння, відображення чи як частина UI.

## Як це зробити:
```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize = map toUpper

-- Використання функції
main :: IO ()
main = putStrLn (capitalize "привіт, як справи?")
```
Вивід:
```
ПРИВІТ, ЯК СПРАВИ?
```

## Пірнання в глибину
Перетворення рядка на великі літери - це стандартна операція в більшості мов програмування. У Haskell, це зазвичай робиться через `map` та функцию `toUpper` із модуля `Data.Char`. Альтернативно, можна написати свій власний механізм з використанням рекурсії або спискових включень. Підхід з `map` мінімалістичний та функціональний, він ефективно трансформує кожний символ в рядку.

## Дивіться ще
- [Haskell `Data.Char` documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html)
- [Learn You a Haskell for Great Good! on `maps` and `filters`](http://learnyouahaskell.com/higher-order-functions#maps-and-filters)
