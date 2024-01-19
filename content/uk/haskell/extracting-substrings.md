---
title:                "Видобування підрядків"
html_title:           "C++: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що і чому?

Витягання підрядків - це процес знаходження та копіювання певної частини рядка. Програмісти використовують це для обробки та аналізу рядків та для праці з форматами текстових файлів, таких як CSV або JSON.

## Як це зробити?

Нижче наведено приклад використання вбудованої функції Haskell `drop`, що використовується для витягування підрядків.

```Haskell
substring :: Int -> Int -> String -> String
substring start end = drop start . take end

main = do
    let str = "Привіт, світе!"
    print $ substring 8 13 str
```

У цьому прикладі, функція `substring` використовує `drop` та `take` для створення підрядка з переданого рядка. Ви можете бачити, як ми беремо підрядок від 8 до 13 індексу рядка `str`, що друкує `"світе"`.

## Детальніше

Якщо говорити про історію, витягування підрядків завжди було важливим інструментом для роботи з рядками в більшості мов програмування.

Щодо альтернатив, можливо використовувати зовнішні бібліотеки, такі як `text` або `bytestring`, які можуть надати більші можливості і краще обробляти великі об'єми тексту.

Настільки важлива частина обробки рядків, витягнення підрядків розміщено прямо в бібліотеках основних рядків Haskell. Це забезпечує оптимальну швидкість та ефективність у порівнянні з ручною реалізацією.

## Посилання

1. [Руководство по строкам в Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/strings)
2. [Вбудовані функції для обробки рядків в Haskell](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-String.html)
3. [Працюйте з рядками в Haskell, використовуючи бібліотеку `text`](https://hackage.haskell.org/package/text)
4. [Efficient string handling in Haskell](http://book.realworldhaskell.org/read/efficient-string-processing.html)