---
title:                "Haskell: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Написання програм звичайно потребує обміну даними з використанням різних форматів. На приклад, в текстових форматах імена зазвичай з позначено з великої літери. Використання функції, що ставить прописні літери у рядках, може значно полегшити особі роботу з такими даними.

## Як виконати це у Haskell

Для того, щоб перетворити рядок так, щоб перша літера була великою, можна використовувати функцію `toUpper` з модуля `Data.Char`. Також, можна визначити власну функцію `capitalize` яка буде працювати на основі списку рядків:

```Haskell
import Data.Char (toUpper)

capitalize :: [String] -> [String]
capitalize = map $ \word -> toUpper (head word) : tail word

main :: IO ()
main = do
    putStrLn "Введіть рядок:"
    input <- getLine
    let capitalized = capitalize (words input)
    putStrLn (unwords capitalized)
```

### Вихідні дані

Введіть рядок: це назва кардінальних пунктів

Це Назва Кардінальних Пунктів

## Глибинний аналіз

Функції `map` та `words` є стандартними функціями у Haskell. Однак, функція `capitalize` визначена за допомогою анонімної функції, яка робить `toUpper` на першому елементі listu, що повертає капіталізоване слово.

## Подивіться також

- [Haskell Strings and Characters](https://wiki.haskell.org/String)
- [Haskell Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html)