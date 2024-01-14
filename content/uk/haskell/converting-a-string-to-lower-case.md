---
title:    "Haskell: Перетворення рядка в нижній регістр"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Часто в процесі написання програм, нам потрібно працювати з рядками тексту. Іноді необхідно привести ці рядки до одного стандартного формату для подальшої обробки. У цьому випадку, помінявши всі великі літери на малі, ми отримаємо єдиний формат для всіх рядків, що буде полегшувати їх подальшу обробку. 

## Як це зробити

Існує багато способів переведення рядка до нижнього регістру в мові програмування Haskell. Розглянемо декілька з них.

```Haskell
import Data.Char (toLower)

-- використання функції toLower
toLowerString :: String -> String
toLowerString = map toLower

-- використання конструкції do
toLowerString2 :: String -> String
toLowerString2 str = do
    char <- str
    return (toLower char)

-- використання рекурсії
toLowerString3 :: String -> String
toLowerString3 [] = []
toLowerString3 (x:xs) = toLower x : toLowerString3 xs
```

Приклад використання функції ```toLowerString```:

```Haskell
main :: IO ()
main = do
    let myString = "Привіт, Haskell!"
    putStrLn $ toLowerString myString

-- output:
-- привіт, haskell!
```

Приклад використання конструкції ```do```:

```Haskell
main :: IO ()
main = do
    let myString = "Привіт, Haskell!"
    putStrLn $ toLowerString2 myString

-- output:
-- привіт, haskell!
```

Приклад використання рекурсії:

```Haskell
main :: IO ()
main = do
    let myString = "Привіт, Haskell!"
    putStrLn $ toLowerString3 myString

-- output:
-- привіт, haskell!
```

## Поглиблення

У мові Haskell існує багато вбудованих і бібліотечних функцій для роботи з рядками. Наприклад, функція ```toLower``` з модуля ```Data.Char``` приймає у себе символ і повертає його в нижньому регістрі. Також, існує функція ```toLower``` з модуля ```Data.Text``` для роботи з Unicode символами.

У нашому прикладі ми використовували функцію ```map```, щоб застосувати функцію ```toLower``` до кожного символу рядка. Ця функція знаходиться у модулі ```Prelude```, який автоматично підключається у кожному проекті.

Крім цього, у Haskell існує також інтересна функція ```putStrLn```, яка приймає строку і виводить її на екран. Ця функція є членом класу типів ```Show```, що дозволяє зберігати вивід даних на консоль.

## Дивись також

- [Документація з функцією toLower з модуля Data.Char](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char