---
title:                "Виділення підрядків"
date:                  2024-01-20T17:46:28.333775-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виділення підрядків"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Витягування підрядків – це процес отримання фрагментів тексту з більших рядків. Розробники роблять це для аналізу тексту, обробки даних чи просто для виклику значущої частини інформації.

## Як робити:
У Haskell можна витягувати підрядки, використовуючи функції `take`, `drop`, `splitAt` та срізи списків. Ось базові приклади:

```Haskell
main :: IO ()
main = do
  let text = "Привіт, як справи?"

  -- Використання take
  print $ take 6 text  -- "Привіт"

  -- Використання drop
  print $ drop 7 text  -- "як справи?"

  -- Використання splitAt
  let (start, end) = splitAt 7 text
  print start  -- "Привіт,"
  print end    -- "як справи?"

  -- Використання срізів списків (це не стандарт Haskell, але бібліотека)
  -- Наприклад, можемо використати бібліотеку Data.List.Split (див. розділ "Див. також")
```

## Поглиблений розгляд:
В Haskell, як і в багатьох інших функціональних мовах, рядки є переліками символів. Текстові операції часто ґрунтуються на операціях зі списками.

Традиційно, Haskell використовує "ліниву оцінку", що означає, що елементи рядків обробляються лише тоді, коли вони потрібні. Це робить витягування підрядків відносно ефективним, оскільки ви не обробляєте весь текст.

Більше того, існують бібліотеки, як `Data.Text` для оптимізованої роботи з текстом та `Data.List.Split` для розширених операцій розділення. Ці бібліотеки надають зручні функції для маніпуляції рядками. Наприклад, `Data.List.Split` може розділити рядки за розділювачами та по індексу.

## Див. також:
- [Hackage: Data.Text library](https://hackage.haskell.org/package/text)
- [Hackage: Data.List.Split library](https://hackage.haskell.org/package/split)
- [Haskell Wiki: Рядки](https://wiki.haskell.org/Strings)
- [Learn You a Haskell for Great Good! - Starting Out (Chapter on Lists)](http://learnyouahaskell.com/starting-out#an-intro-to-lists)

Ці ресурси можуть допомогти вам краще зрозуміти текстові операції в Haskell і практично їх застосувати.