---
title:                "Расчет даты в будущем или прошлом"
date:                  2024-01-28T23:56:29.641720-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elm/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Расчёт будущей или прошлой даты — это всего лишь корректировка заданной даты на определённый промежуток времени. Программисты делают это для работы со сроками, событиями, напоминаниями — со всем, что связано с датами.

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
