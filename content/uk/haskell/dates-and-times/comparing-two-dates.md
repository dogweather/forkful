---
title:                "Порівняння двох дат"
aliases:
- /uk/haskell/comparing-two-dates.md
date:                  2024-01-20T17:33:38.249737-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що та чому?
Порівнюємо дві дати, щоб визначити, яка з них раніше чи пізніше, або чи вони однакові. У програмуванні це важливо для обробки термінів, планування заходів, ведення журналів тощо.

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
