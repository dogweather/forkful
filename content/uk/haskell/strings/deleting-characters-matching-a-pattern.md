---
date: 2024-01-20 17:42:25.903096-07:00
description: "How to: | \u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Output."
lastmod: '2024-04-05T22:38:48.378322-06:00'
model: gpt-4-1106-preview
summary: "| \u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438: Output."
title: "\u0412\u0438\u0434\u0430\u043B\u0435\u043D\u043D\u044F \u0441\u0438\u043C\u0432\
  \u043E\u043B\u0456\u0432 \u0437\u0430 \u0432\u0456\u0437\u0435\u0440\u0443\u043D\
  \u043A\u043E\u043C"
weight: 5
---

## How to: | Як зробити:
```Haskell
import Data.List (delete)

-- Видалення конкретного символу
removeChar :: Char -> String -> String
removeChar _ ""     = ""
removeChar c (x:xs) 
  | c == x    = removeChar c xs
  | otherwise = x : removeChar c xs

-- Використання функції
main :: IO ()
main = do
  let result = removeChar 'a' "banana"
  putStrLn result
```
Output:
```
bnn
```
Нащадок на `Data.List`, тепер видалимо список символів:

```Haskell
import Data.List (delete)

-- Видалення множини символів
removeChars :: [Char] -> String -> String
removeChars [] str = str
removeChars (c:cs) str = removeChars cs (removeChar c str)

-- Де removeChar визначено як раніше

-- Використання функції
main :: IO ()
main = do
  let result = removeChars "aeiou" "banana"
  putStrLn result
```
Output:
```
bnn
```

## Deep Dive | Поглиблений Аналіз:
У Haskell видалення символів за шаблоном не має вбудованої функції, як у деяких інших мовах. Замість цього пишемо свої функції, як `removeChar` та `removeChars`. Функціональна природа Haskell заохочує до складання менших функцій у більш складні. `removeChar` і `removeChars` використовують рекурсію – типовий підхід у Haskell для ітерації.

Альтернативи включають використання регулярних виразів з бібліотекою `regex` або вбудовані функції високого рівня, як `filter` (дія протилежна до видалення). Видалення за шаблном можна реалізувати і з більшою ефективністю, наприклад, використовуючи структури даних, оптимізовані для цього завдання, як перетворювачі рядків (string transformers).

## See Also | Дивіться Також:
- [Haskell.org Book](http://haskellbook.com/) – глибоке занурення у мову Haskell.
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) – доступний ресурс для вивчення Haskell.
- [Hoogle](https://hoogle.haskell.org/) – пошукова система для Haskell бібліотек.
- [Stack Overflow](https://stackoverflow.com/questions/tagged/haskell) – спільнота з відповідями на питання з програмування на Haskell.
