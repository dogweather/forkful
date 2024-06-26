---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:57.360438-07:00
description: "\u041A\u0430\u043A: \u0412 Clojure \u0432\u044B \u0432 \u043E\u0441\u043D\
  \u043E\u0432\u043D\u043E\u043C \u0431\u0443\u0434\u0435\u0442\u0435 \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\
  \u0438\u043E\u0442\u0435\u043A\u0443 `clj-time` \u0434\u043B\u044F \u043E\u043F\u0435\
  \u0440\u0430\u0446\u0438\u0439 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438. \u0412\
  \u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u043F\u0440\u0438\u043C\
  \u0435\u0440."
lastmod: '2024-03-13T22:44:44.374880-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Clojure \u0432\u044B \u0432 \u043E\u0441\u043D\u043E\u0432\u043D\u043E\
  \u043C \u0431\u0443\u0434\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\
  \u0437\u043E\u0432\u0430\u0442\u044C \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0443 `clj-time` \u0434\u043B\u044F \u043E\u043F\u0435\u0440\u0430\u0446\u0438\
  \u0439 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438."
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
weight: 26
---

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
