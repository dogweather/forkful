---
title:                "Об'єднання рядків"
aliases: - /uk/haskell/concatenating-strings.md
date:                  2024-01-20T17:35:25.325052-07:00
model:                 gpt-4-1106-preview
simple_title:         "Об'єднання рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і чому?

Конкатенація рядків – це процес з'єднання двох або більше рядків в один. Програмісти роблять це для створення повідомлень, динамічного формування коду і просто для того, щоб об'єднати інформацію.

## Як це зробити:

```Haskell
-- Спосіб 1: Використання оператора (++)
hello :: String
hello = "Привіт"

world :: String
world = "Світ"

main :: IO ()
main = putStrLn (hello ++ ", " ++ world ++ "!")

-- Вивід: Привіт, Світ!
```

```Haskell
-- Спосіб 2: Використання функції concat
parts :: [String]
parts = ["Привіт", ", ", "Світ", "!"]

main :: IO ()
main = putStrLn (concat parts)

-- Вивід: Привіт, Світ!
```

## В глибину:

Історичний контекст: В Haskell конкатенація рядків вважається базовою операцією і її використання може бути простежено аж до його функціональних корінів в Lambda-рахунку.

Альтернативи: В Haskell існують різні шляхи конкатенації. Оператор `(++)` простий, але при конкатенації великої кількості рядків він може бути неефективним. Функції як `concat`, `unwords`, або `intercalate` з модуля `Data.List` надають більшу гнучкість та ефективність.

Деталі реалізації: У Haskell рядки внутрішньо представлені як списки символів (`[Char]`), тому конкатенація рядків часто відбувається так само, як і конкатенація списків.

## Подивіться також:

- [Learn You a Haskell for Great Good: Starting Out](http://learnyouahaskell.com/starting-out#im-a-list-comprehension)
- [Real World Haskell: Chapter 4. Functional Programming](http://book.realworldhaskell.org/read/functional-programming.html)
