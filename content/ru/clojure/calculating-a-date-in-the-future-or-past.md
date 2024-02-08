---
title:                "Расчет даты в будущем или прошлом"
aliases:
- ru/clojure/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T23:55:57.360438-07:00
model:                 gpt-4-0125-preview
simple_title:         "Расчет даты в будущем или прошлом"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Зачем?

Расчет будущих или прошлых дат включает в себя манипуляции с датами, чтобы узнать, какими они будут после определенного периода, или какими они были. Программисты делают это для таких вещей, как планирование событий, напоминаний или вычисление сроков истечения.

## Как:

В Clojure вы в основном будете использовать библиотеку `clj-time` для операций с датами. Вот быстрый пример:

```clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])
(require '[clj-time.periodic :as periodic])

;; Добавить 5 дней к текущей дате
(let [now (time/now)
      five-days (time/plus now (time/days 5))]
  (str "Через пять дней: " (coerce/to-string five-days)))

;; Вычесть 10 дней из конкретной даты
(let [specific-date (coerce/to-date-time "2023-03-01T12:00:00.000Z")
      ten-days-ago (time/minus specific-date (time/days 10))]
  (str "Десять дней до 1 марта 2023: " (coerce/to-string ten-days-ago)))
```

Пример вывода:
```
"Через пять дней: 2023-03-23Т08:00:00.000Z"
"Десять дней до 1 марта 2023: 2023-02-19Т12:00:00.000Z"
```

## Подробнее

В прошлые дни программисты использовали классы `Date` и `Calendar` Java. Но, давайте будем честными, это головная боль — громоздкие и склонные к ошибкам. Библиотека `clj-time` принесла некоторый порядок, обернув более дружественный к разработчику API Joda-Time.

Альтернативы? Java 8 ввела `java.time` (JSR-310), которая довольно хороша, но в мире Clojure мы все еще уютно с `clj-time`.

При расчете дат вы используете периоды для понятий вроде "дни" и "месяцы" и продолжительности для точных счетов миллисекунд. Имейте в виду часовые пояса — даты и времена могут драматически изменяться в зависимости от правил часового пояса, а летнее время (DST) может внести свои коррективы.

## Смотрите также

- Репозиторий GitHub `clj-time`: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- `java-time` для Clojure: [https://github.com/dm3/clojure.java-time](https://github.com/dm3/clojure.java-time)
