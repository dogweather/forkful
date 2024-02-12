---
title:                "Інтерполяція рядків"
aliases:
- /uk/haskell/interpolating-a-string.md
date:                  2024-01-20T17:51:44.839060-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Інтерполяція рядків - це вбудовування змінних чи виразів у стрічку. Програмісти використовують це для зручності форматування тексту та динамічного створення рядків.

## Як це зробити:
У Haskell інтерполяція рядків не є вбудованою, але можна використовувати бібліотеку `text` із шаблонними літералами. 

```Haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

import Data.Text
import Data.String.Interpolate ( i )

main :: IO ()
main = do
  let userName = "Володимир"
  let userEmail = "[email protected]"
  putStrLn [i|Користувач: #{userName}, Електронна адреса: #{userEmail}|]
```

Запуск програми виведе:

```
Користувач: Володимир, Електронна адреса: [email protected]
```

Також бібліотека `printf` може слугувати для форматування рядків.

```Haskell
import Text.Printf ( printf )

main :: IO ()
main = do
  let age = 30
  printf "Мені %d років\n" age
```

Вивід буде:

```
Мені 30 років
```

## Занурення в глибину
Haskell не має вбудованої підтримки для інтерполяції рядків, на відміну від мов як Python чи JavaScript. Історично, одна з причин – це типова система Haskell та немутабельність даних, які не сприяють простій встроєній інтерполяції. 

Альтернативами вбудованій інтерполяції є: використання `printf` з модуля `Text.Printf`, що дозволяє форматування рядків, чи бібліотеки та пакети на зразок `interpolate`, `shakespeare`, і `formatting`.

`text` і `interpolate` використовують quasi-quoters, які дають змогу створювати власні обчислювальні контексти u Haskell, внаслідок чого можливою стає інтерполяція.

## Ще деякі ресурси
- [Hackage: text](https://hackage.haskell.org/package/text) - документація бібліотеки `text`.
- [Hackage: interpolate](https://hackage.haskell.org/package/interpolate) - інформація про бібліотеку `interpolate`.
- [Haskell Wiki: String interpolation](https://wiki.haskell.org/String_interpolation) - стаття з вікіпедії Haskell про інтерполяцію рядків.
- [Hackage: format](https://hackage.haskell.org/package/formatting) - документація бібліотеки `formatting`.
