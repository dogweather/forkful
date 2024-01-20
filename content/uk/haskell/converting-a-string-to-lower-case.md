---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення рядка в нижній регістр - це процес зміни всіх символів верхнього регістру на їх відповідники в нижньому регістрі. Програмісти роблять це, щоб спростити порівняння та пошук рядків.

## Як це зробити:

```Haskell
import Data.Char (toLower)

lowerCaseStr :: String -> String
lowerCaseStr = map toLower
```

Приклад використання і виводу:

```Haskell
main = do
    let upperStr = "HELLO, WORLD!"
    print $ lowerCaseStr upperStr
```
Виведе:
```
"hello, world!"
```
## Поглиблений аналіз

1. Історичний контекст: Методи перетворення рядків до нижнього регістру існують давно в багатьох мовах програмування, включаючи C, Java, Python та інші. Haskell, як функціональна мова, також підтримує цю функцію.

2. Альтернативи: Ви також можете використовувати бібліотеку `Text` для обробки рядків та переводу їх в низький регістр.

```Haskell
import Data.Text (toLower, pack, unpack)

lowerCaseStrT :: String -> String
lowerCaseStrT = unpack . toLower . pack
```
3. Деталі реалізації: Функція `toLower` з модуля `Data.Char` визначає, як перетворюється кожен символ. Вона використовує відповідне відображення в таблиці Unicode, щоб знайти відповідний символ нижнього регістру.

## Також дивіться

1. Haskell Documentation: [Data.Char](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
2. Learn You a Haskell for Great Good: [Modules](http://learnyouahaskell.com/modules)
3. School of Haskell: [Working with Text](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/working-with-text)