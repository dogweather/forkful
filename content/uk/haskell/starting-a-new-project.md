---
title:                "Початок нового проєкту"
html_title:           "Elm: Початок нового проєкту"
simple_title:         "Початок нового проєкту"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що і Навіщо?
Початок нового проекту - це створення чернетки програмного коду для ваших ідей. Програмісти роблять це, щоб переходити від теоретичних роздумів до реальної роботи з кодом.

## Як це робити:
Ось невеликий приклад коду, який демонструє, як ви можете створити новий проект в Haskell:

```Haskell
{-# LANGUAGE TypeApplications #-}

module Main where

import Text.Printf

main :: IO ()
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  printf "Hi, %s, you've just started a new Haskell project!\n" name
```
Вищенаведений код спитає вас про ваше ім'я, а потім виведе повідомлення, поздоровляючи вас з початком нового проекту на Haskell.

## Глибше в тему
Haskell є чистою функціональною мовою, вперше представленою в 1990 році. Хоча початок нового проекту зазвичай є простим, є інші мови, наприклад, Python або JavaScript, де процес створення нового проекту може бути більш систематизованим. Втім, це лише наголошує на тому, що Haskell дозволяє вам почати програмування якомога швидше, без встановлення багатох бібліотек або фреймворків.

## Дивіться також:
Для більш глибокого вивчення рекомендуємо вам ці ресурси:
- *Learn You a Haskell for Great Good!*: [http://learnyouahaskell.com/](http://learnyouahaskell.com/)
- *Real World Haskell*: [http://book.realworldhaskell.org/](http://book.realworldhaskell.org/)