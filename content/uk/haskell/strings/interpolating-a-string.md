---
date: 2024-01-20 17:51:44.839060-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Haskell \u0456\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\
  \u044F \u0440\u044F\u0434\u043A\u0456\u0432 \u043D\u0435 \u0454 \u0432\u0431\u0443\
  \u0434\u043E\u0432\u0430\u043D\u043E\u044E, \u0430\u043B\u0435 \u043C\u043E\u0436\
  \u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\
  \u0430\u0442\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443\
  \ `text` \u0456\u0437 \u0448\u0430\u0431\u043B\u043E\u043D\u043D\u0438\u043C\u0438\
  \ \u043B\u0456\u0442\u0435\u0440\u0430\u043B\u0430\u043C\u0438."
lastmod: '2024-03-13T22:44:49.334262-06:00'
model: gpt-4-1106-preview
summary: "\u0423 Haskell \u0456\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\
  \u0456\u044F \u0440\u044F\u0434\u043A\u0456\u0432 \u043D\u0435 \u0454 \u0432\u0431\
  \u0443\u0434\u043E\u0432\u0430\u043D\u043E\u044E, \u0430\u043B\u0435 \u043C\u043E\
  \u0436\u043D\u0430 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\
  \u0432\u0430\u0442\u0438 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0443\
  \ `text` \u0456\u0437 \u0448\u0430\u0431\u043B\u043E\u043D\u043D\u0438\u043C\u0438\
  \ \u043B\u0456\u0442\u0435\u0440\u0430\u043B\u0430\u043C\u0438."
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 8
---

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
