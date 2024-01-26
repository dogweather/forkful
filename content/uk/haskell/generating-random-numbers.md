---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:31.272434-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Генерування випадкових чисел — це процес створення чисел, які не можна передбачити. Програмісти використовують цей процес для імітації випадкових подій, тестування, ігор та криптографії.

## Як це зробити:
Haskell використовує модуль `System.Random` для генерування випадкових чисел. Система Та ось код для початку:

```Haskell
import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    let (randNumber, _) = randomR (1, 100) gen :: (Int, StdGen)
    print randNumber
```

Коли виконаєте, виведе щось на кшталт:

```
29
```

Кожен запуск — нове число.

## Поглиблено:
Генератор випадкових чисел в Haskell — це лінивий і прозорий. Історично, Haskell використовував лінійні конгруентні генератори, але зараз стандартною є Mersenne Twister. Альтернатива — `System.Random.MWC`.

Що стосується впровадження, Haskell розділяє джерело випадковості (`StdGen`) від функцій, що генерують випадкові значення (`random`, `randomR`). Це дозволяє контролювати значення для відтворюваності тестів.

## Див. також:
- Документація по `System.Random`: http://hackage.haskell.org/package/random
- Книга "Learn You a Haskell for Great Good!": http://learnyouahaskell.com
- Блог про Mersenne Twister та його альтернативи: https://www.serpentine.com/blog/2014/05/19/random-numbers/
