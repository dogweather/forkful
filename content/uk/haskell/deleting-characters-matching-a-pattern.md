---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:42:25.903096-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? | Що і Чому?
Видалення символів за шаблоном — це процес відбору і видалення певних символів із рядка. Програмісти роблять це, аби очистити дані, відсіяти непотрібне або змінити форматування.

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
