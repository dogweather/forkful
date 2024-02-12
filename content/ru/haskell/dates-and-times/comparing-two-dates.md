---
title:                "Сравнение двух дат"
aliases:
- /ru/haskell/comparing-two-dates.md
date:                  2024-01-28T23:56:31.571496-07:00
model:                 gpt-4-0125-preview
simple_title:         "Сравнение двух дат"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/haskell/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Сравнение двух дат заключается в том, чтобы определить, какая из них раньше, позже или если они соответствуют одному и тому же моменту времени. Программисты делают это для сортировки событий, определения продолжительности и управления логикой, зависящей от времени.

## Как это сделать:

Haskell, известный своей чистотой, требует от вас общения на языке дат с правильными библиотеками. Давайте используем `Data.Time`.

```haskell
import Data.Time

-- Определение двух дат
date1 :: UTCTime
date1 = UTCTime (fromGregorian 2023 4 1) (secondsToDiffTime 0)

date2 :: UTCTime
date2 = UTCTime (fromGregorian 2024 4 2) (secondsToDiffTime 3600)

-- Сравнение дат
compareDates :: UTCTime -> UTCTime -> Ordering
compareDates = compare

main :: IO ()
main = do
    print $ date1 `compareDates` date2 -- Вывод будет LT
    print $ date2 `compareDates` date1 -- Вывод будет GT
    print $ date1 `compareDates` date1 -- Вывод будет EQ
```

Просто, правда? `LT` для меньше чем, `GT` для больше чем, и `EQ` для равно.

## Глубже в тему

В прошлом, управление временем в Haskell не было таким гладким. Мы обязаны нашим текущим удобствам прогрессу библиотеки `Data.Time` на протяжении лет. Она предлагает нам `UTCTime`, счастливо однозначную точку во времени.

Альтернативы? Конечно. Вам могут пригодиться `Data.Time.Calendar` и `Data.Time.Clock` для конкретных сценариев. Также есть старая библиотека `time` для тех, кто ностальгирует или работает с устаревшим кодом.

Теперь к сути: Сравнение дат в Haskell основывается на `UTCTime`, который сочетает в себе день (`Day`) и время (`DiffTime` или `NominalDiffTime`). Здесь важную роль играет функция `compare`, которая является активным участником класса `Ord`, позволяя нам использовать `>, <, ==` и так далее. Просто помните, что Haskell любит свою типобезопасность. Убедитесь, что вы всегда сравниваете яблоки с яблоками, или, в нашем случае, `UTCTime` с `UTCTime`.

## Смотрите также

Углубитесь в тему или найдите помощь с этими ресурсами:
- [Пакет `Data.Time` на Hackage](https://hackage.haskell.org/package/time-1.11/docs/Data-Time.html)
- [Learn You a Haskell for Great Good! – Для нежного введения](http://learnyouahaskell.com/)
- [Stack Overflow для решения реальных проблем](https://stackoverflow.com/questions/tagged/haskell+time)
