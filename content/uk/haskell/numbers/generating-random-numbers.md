---
date: 2024-01-27 20:34:32.478755-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0414\u043B\u044F \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u0457 \u0432\
  \u0438\u043F\u0430\u0434\u043A\u043E\u0432\u0438\u0445 \u0447\u0438\u0441\u0435\u043B\
  \ \u0432 Haskell, \u0437\u0430\u0437\u0432\u0438\u0447\u0430\u0439 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\u044F \u043F\
  \u0430\u043A\u0435\u0442 `random`, \u044F\u043A\u0438\u0439 \u0454 \u0447\u0430\u0441\
  \u0442\u0438\u043D\u043E\u044E Haskell Platform. \u041E\u0441\u044C \u043A\u0440\
  \u043E\u043A-\u0437\u0430-\u043A\u0440\u043E\u043A\u043E\u043C\u2026"
lastmod: '2024-03-13T22:44:49.351204-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u0457\
  \ \u0432\u0438\u043F\u0430\u0434\u043A\u043E\u0432\u0438\u0445 \u0447\u0438\u0441\
  \u0435\u043B \u0432 Haskell, \u0437\u0430\u0437\u0432\u0438\u0447\u0430\u0439 \u0432\
  \u0438\u043A\u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0454\u0442\u044C\u0441\
  \u044F \u043F\u0430\u043A\u0435\u0442 `random`, \u044F\u043A\u0438\u0439 \u0454\
  \ \u0447\u0430\u0441\u0442\u0438\u043D\u043E\u044E Haskell Platform."
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u044F \u0432\u0438\u043F\u0430\
  \u0434\u043A\u043E\u0432\u0438\u0445 \u0447\u0438\u0441\u0435\u043B"
weight: 12
---

## Як це зробити:
Для генерації випадкових чисел в Haskell, зазвичай використовується пакет `random`, який є частиною Haskell Platform. Ось крок-за-кроком інструкція:

Спершу, переконайтеся, що у вас встановлений пакет `random`. Якщо ні, ви можете отримати його через Cabal або Stack.

### Генерація випадкового числа
Для генерації простого випадкового числа, ви можете використовувати функцію `randomRIO`, яка виробляє випадкове значення в межах вказаного діапазону.

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Випадкове число: " ++ show randomNumber
```

### Генерація списку випадкових чисел
Генерація списку випадкових чисел є трохи складнішою, але все ще простою:

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n-1)
  return (r:rs)

main :: IO ()
main = do
  numbers <- randomList 5
  print numbers
```

Цей фрагмент коду створює функцію `randomList`, яка генерує список випадкових цілих чисел. Замініть `(1, 100)` на бажаний вами діапазон.

## Поглиблений розгляд
Пакет `random` в Haskell надає псевдовипадковий генератор чисел (PRNG), що означає, що згенеровані числа не є справді випадковими, але можуть здаватися випадковими для багатьох застосунків. Основа здатності Haskell до генерації випадкових чисел полягає в типі класу `RandomGen`, який абстрагує різні методи генерації випадкових чисел, та типі класу `Random`, який включає типи, що можуть бути згенеровані випадковим чином.

Історично, підхід Haskell до генерації випадкових чисел підкреслював чистоту та відтворюваність. Саме тому операції, які включають випадковість, явно обробляються в монаді `IO` або вимагають вручну передавання та оновлення станів генератора — для збереження референційної прозорості.

В певних застосунках, таких як криптографія, псевдовипадкові числа, згенеровані за допомогою стандартного PRNG, можуть не бути досить безпечними. Для цих цілей програмісти Haskell часто звертаються до більш спеціалізованих бібліотек, таких як `crypto-random`, які розроблені, щоб задовольняти суворі вимоги криптографічних застосунків.

Більше того, альтернативні бібліотеки, як-от `mwc-random`, пропонують кращу продуктивність та якість випадкових чисел для симуляцій та інших застосунків, імплементуючи сучасні алгоритми, такі як Mersenne Twister.

Обираючи підхід до генерації випадкових чисел в Haskell, важливо взяти до уваги потреби застосунку щодо якості випадковості, продуктивності та безпеки, аби вибрати найбільш відповідний інструмент або бібліотеку.
