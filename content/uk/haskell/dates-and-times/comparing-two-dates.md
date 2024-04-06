---
date: 2024-01-20 17:33:38.249737-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Haskell, \u043F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\
  \u0430\u0442 \u0437\u0430\u0441\u043D\u043E\u0432\u0430\u043D\u0435 \u043D\u0430\
  \ \u0442\u0438\u043F\u0456 `UTCTime` \u0437 \u043F\u0430\u043A\u0435\u0442\u0443\
  \ `time`, \u044F\u043A\u0438\u0439 \u0432\u0436\u0435 \u0434\u0430\u0432\u043D\u043E\
  \ \u0454 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043E\u043C \u0434\u043B\
  \u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0447\u0430\u0441\u0443 \u0456\
  \ \u0434\u0430\u0442. \u0410\u043B\u044C\u0442\u0435\u0440\u043D\u0430\u0442\u0438\
  \u0432\u0438\u2026"
lastmod: '2024-04-05T22:51:02.449445-06:00'
model: gpt-4-1106-preview
summary: "\u0423 Haskell, \u043F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F\
  \ \u0434\u0430\u0442 \u0437\u0430\u0441\u043D\u043E\u0432\u0430\u043D\u0435 \u043D\
  \u0430 \u0442\u0438\u043F\u0456 `UTCTime` \u0437 \u043F\u0430\u043A\u0435\u0442\u0443\
  \ `time`, \u044F\u043A\u0438\u0439 \u0432\u0436\u0435 \u0434\u0430\u0432\u043D\u043E\
  \ \u0454 \u0441\u0442\u0430\u043D\u0434\u0430\u0440\u0442\u043E\u043C \u0434\u043B\
  \u044F \u043E\u0431\u0440\u043E\u0431\u043A\u0438 \u0447\u0430\u0441\u0443 \u0456\
  \ \u0434\u0430\u0442."
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Як це зробити:
```Haskell
import Data.Time

-- Порівнюємо дві дати
compareDates :: IO ()
compareDates = do
    let date1 = fromGregorian 2023 3 15 -- 15 березня 2023
    let date2 = fromGregorian 2023 10 19 -- 19 жовтня 2023
    let comparisonResult = compare date1 date2
    printResult comparisonResult

-- Друкуємо результат порівняння
printResult :: Ordering -> IO ()
printResult GT = putStrLn "Перша дата пізніше другої."
printResult LT = putStrLn "Перша дата раніше другої."
printResult EQ = putStrLn "Дати однакові."

main :: IO ()
main = compareDates
```
Виведення:
```
Перша дата раніше другої.
```

## Поглиблений аналіз:
У Haskell, порівняння дат засноване на типі `UTCTime` з пакету `time`, який вже давно є стандартом для обробки часу і дат. Альтернативи включають сторонні бібліотеки як `thyme` чи `chronos`, але `time` найчастіше за все вповні покриває потреби розробників.

Функція `fromGregorian` використовує Григоріанський календар, dominantly used since 1582, and the `compare` function can directly compare two dates. The result is of the type `Ordering`, with possible values `GT` (greater than), `LT` (less than), or `EQ` (equal to).

Детальнішу інформацію про обробку дат і часу в Haskell можна знайти в офіційній документації пакету `time`.

## Додатково:
- Офіційна документація `time`: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
