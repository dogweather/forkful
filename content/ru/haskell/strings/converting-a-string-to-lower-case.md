---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:50.744433-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Haskell \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\
  \ \u043C\u043E\u0434\u0443\u043B\u044C `Data.Char` \u0434\u043B\u044F \u043C\u0430\
  \u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\u0439 \u0441 \u0441\u0438\u043C\
  \u0432\u043E\u043B\u0430\u043C\u0438. \u0424\u0443\u043D\u043A\u0446\u0438\u044F\
  \ `toLower` \u0441\u043F\u0435\u0446\u0438\u0430\u043B\u044C\u043D\u043E \u0438\u0437\
  \u043C\u0435\u043D\u044F\u0435\u0442 \u043E\u0434\u0438\u043D \u0441\u0438\u043C\
  \u0432\u043E\u043B \u043D\u0430 \u043D\u0438\u0436\u043D\u0438\u0439 \u0440\u0435\
  \u0433\u0438\u0441\u0442\u0440.\u2026"
lastmod: '2024-03-13T22:44:45.109915-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442 \u043C\
  \u043E\u0434\u0443\u043B\u044C `Data.Char` \u0434\u043B\u044F \u043C\u0430\u043D\
  \u0438\u043F\u0443\u043B\u044F\u0446\u0438\u0439 \u0441 \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0430\u043C\u0438."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u043D\u0438\u0436\u043D\u0438\
  \u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 4
---

## Как это сделать:
Haskell использует модуль `Data.Char` для манипуляций с символами. Функция `toLower` специально изменяет один символ на нижний регистр. Вы должны применить эту функцию к строке, чтобы полностью преобразовать её в нижний регистр. Смотрите пример кода:

```haskell
import Data.Char (toLower)

-- Преобразование строки в нижний регистр
lowercaseString :: String -> String
lowercaseString = map toLower

-- Использование
main :: IO ()
main = putStrLn $ lowercaseString "Hello, Haskell!"
```

Пример вывода:

```
hello, haskell!
```

## Глубокое погружение
Исторически концепция регистров букв происходит из эпохи ручной полиграфии, когда буквы верхнего и нижнего регистра хранились в отдельных ящиках. В программировании преобразование регистра обеспечивает единообразие, особенно в операциях, не чувствительных к регистру.

Вот краткое изложение по особенностям Haskell. Модуль `Data.Char`, в котором находится `toLower`, был представлен в стандарте Haskell 98. С тех пор он остаётся основным средством для манипуляций с символами. Другие языки имеют свои методы, например, `.toLowerCase()` в JavaScript или `.lower()` в Python, но в Haskell `map` и `toLower` аккуратно решают эту задачу.

Внутри `toLower` учитывает Unicode, что означает, что он может обрабатывать огромный репертуар символов и скриптов за пределами базового диапазона ASCII – это полезно для интернационализации.

Альтернативы? Конечно, вы могли бы создать свою собственную функцию, имитирующую `toLower`, но зачем изобретать велосипед? Придерживайтесь `Data.Char` для читабельности и надёжности. Кроме того, библиотеки, такие как `text` и `bytestring`, предлагают более производительные подходы, если вы работаете с большими наборами данных или стремитесь к повышению производительности.

## Смотрите также
- Документация `Data.Char`: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html
- Отчёт Haskell 98 о `Data.Char`: https://www.haskell.org/onlinereport/standard-prelude.html
- Библиотека Text для Haskell: https://hackage.haskell.org/package/text
- Библиотека ByteString для Haskell: https://hackage.haskell.org/package/bytestring
