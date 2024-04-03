---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:29.641720-07:00
description: "\u041A\u0430\u043A: \u041C\u043E\u0434\u0443\u043B\u044C `Time` \u0432\
  \ Elm \u0438 \u043F\u0430\u043A\u0435\u0442 `justinmimbs/time-extra` \u043F\u043E\
  \u0437\u0432\u043E\u043B\u044F\u044E\u0442 \u043D\u0430\u043C \u043B\u0435\u0433\
  \u043A\u043E \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u0438\u0440\u043E\u0432\u0430\
  \u0442\u044C \u0434\u0430\u0442\u0430\u043C\u0438."
lastmod: '2024-03-13T22:44:44.926317-06:00'
model: gpt-4-0125-preview
summary: "\u041C\u043E\u0434\u0443\u043B\u044C `Time` \u0432 Elm \u0438 \u043F\u0430\
  \u043A\u0435\u0442 `justinmimbs/time-extra` \u043F\u043E\u0437\u0432\u043E\u043B\
  \u044F\u044E\u0442 \u043D\u0430\u043C \u043B\u0435\u0433\u043A\u043E \u043C\u0430\
  \u043D\u0438\u043F\u0443\u043B\u0438\u0440\u043E\u0432\u0430\u0442\u044C \u0434\u0430\
  \u0442\u0430\u043C\u0438."
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
weight: 26
---

## Как:
Модуль `Time` в Elm и пакет `justinmimbs/time-extra` позволяют нам легко манипулировать датами.

```Elm
import Time exposing (Posix)
import Time.Extra as TimeExtra

--calculateDate : Int -> Posix -> Posix
-- @deltaDays: количество дней для добавления (отрицательное для вычитания)
-- @fromDate: начальная дата в формате Posix

calculateDate deltaDays fromDate =
    TimeExtra.add TimeExtra.days deltaDays fromDate

-- Использование
-- Не забудьте, Elm считает время в миллисекундах с эпохи Unix.

sampleDate = Time.millisToPosix 1580515200000  -- 1 февраля 2020 г. 00:00:00 UTC
futureDate = calculateDate 10 sampleDate       -- Добавляем 10 дней
pastDate = calculateDate -15 sampleDate         -- Вычитаем 15 дней

-- примеры вывода:
-- futureDate -> 1581552000000  -- 12 февраля 2020 г. 00:00:00 UTC
-- pastDate -> 1580006400000    -- 17 января 2020 г. 00:00:00 UTC
```

## Глубже
В старые времена работа с датами в программировании была сложной. Разные системы, форматы и часовые пояса доставляли всем головную боль. Модуль `Time` в Elm, основанный на системе времени Unix (миллисекунды с 1970 года), стандартизирует это. Пакет `justinmimbs/time-extra` дополнительно упрощает выполнение операций с датами, например, добавление или вычитание дней.

Альтернативы? Другие языки имеют свои собственные библиотеки, например, `datetime` в Python или `Date` в JavaScript. Но подход Elm предлагает строгую типизацию и чистоту, снижая количество ошибок.

Помимо добавления дней, вы также можете работать с месяцами, годами или даже часами и минутами. Функции в Elm и в пакетах вроде `time-extra` сосредоточены на неизменяемости и чистых функциях — это означает отсутствие побочных эффектов. Когда вы рассчитываете новую дату, оригинал остаётся без изменений.

## Смотрите также
- Модуль `Time` в Elm: https://package.elm-lang.org/packages/elm/time/latest/
- Пакет `justinmimbs/time-extra`: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- Руководство по Elm о времени: https://guide.elm-lang.org/effects/time.html
