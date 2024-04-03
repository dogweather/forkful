---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:31.571496-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Haskell, \u0438\u0437\u0432\u0435\u0441\u0442\u043D\u044B\u0439 \u0441\
  \u0432\u043E\u0435\u0439 \u0447\u0438\u0441\u0442\u043E\u0442\u043E\u0439, \u0442\
  \u0440\u0435\u0431\u0443\u0435\u0442 \u043E\u0442 \u0432\u0430\u0441 \u043E\u0431\
  \u0449\u0435\u043D\u0438\u044F \u043D\u0430 \u044F\u0437\u044B\u043A\u0435 \u0434\
  \u0430\u0442 \u0441 \u043F\u0440\u0430\u0432\u0438\u043B\u044C\u043D\u044B\u043C\
  \u0438 \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430\u043C\u0438\
  . \u0414\u0430\u0432\u0430\u0439\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u0443\u0435\u043C `Data.Time`."
lastmod: '2024-03-13T22:44:45.156385-06:00'
model: gpt-4-0125-preview
summary: "Haskell, \u0438\u0437\u0432\u0435\u0441\u0442\u043D\u044B\u0439 \u0441\u0432\
  \u043E\u0435\u0439 \u0447\u0438\u0441\u0442\u043E\u0442\u043E\u0439, \u0442\u0440\
  \u0435\u0431\u0443\u0435\u0442 \u043E\u0442 \u0432\u0430\u0441 \u043E\u0431\u0449\
  \u0435\u043D\u0438\u044F \u043D\u0430 \u044F\u0437\u044B\u043A\u0435 \u0434\u0430\
  \u0442 \u0441 \u043F\u0440\u0430\u0432\u0438\u043B\u044C\u043D\u044B\u043C\u0438\
  \ \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0430\u043C\u0438."
title: "\u0421\u0440\u0430\u0432\u043D\u0435\u043D\u0438\u0435 \u0434\u0432\u0443\u0445\
  \ \u0434\u0430\u0442"
weight: 27
---

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
