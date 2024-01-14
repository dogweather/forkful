---
title:    "Haskell: Читання аргументів командного рядка"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Командний рядок є важливою частиною програмування, особливо у мовах, таких як Haskell. Що ж це таке? І чому би хтось хотів вчитися зчитувати аргументи командного рядка у цій мові програмування? Прочитайте далі, щоб дізнатися!

## Як

Зчитування аргументів командного рядка може бути корисним у багатьох випадках, наприклад, для передачі параметрів у програму чи для керування її роботою. У Haskell це можна зробити дуже просто за допомогою вбудованої функції `getArgs`. Приклад коду та вихідний результат можна побачити нижче:

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Першим аргументом було передано: " ++ args !! 0
    putStrLn $ "Другим аргументом було передано: " ++ args !! 1
```

Вхід: `runhaskell args_example.hs arg1 arg2`

Вихід: 
    Першим аргументом було передано: arg1
    Другим аргументом було передано: arg2

## Глибокий занурення

Як бачимо в прикладі вище, функція `getArgs` повертає список всіх аргументів командного рядка, переданих при запуску програми. Ми можемо отримати доступ до цього списку та обробити його потрібним способом. Також можна використовувати інші функції для зчитування аргументів командного рядка, наприклад, `getProgName` для отримання назви програми.

## Дивись також

- [Документація по `getArgs`](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-Environment.html#g:getArgs)
- [Приклади зчитування аргументів командного рядка](https://www.tutorialspoint.com/haskell/haskell_command_line_arguments.htm)
- [Стаття про роботу з командним рядком у Haskell](https://byorgey.wordpress.com/2011/02/27/learning-haskell-through-escapades-in-the-land-of-prolog-pt-1/)