---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що & чому?
Порівняння двох дат дозволяє нам визначити, яка дата раніша, пізніша або чи вони однакові. Це корисно для обробки подій, що відбулися в період часу або для виконання операцій, що залежать від дат.

## Як це зробити:
Як наступний приклад, давайте порівняємо дві дати за допомогою Clojure.

```Clojure
(require '[clj-time.core :as t]
         '[clj-time.coerce :as c])

(def date1 (t/date-time 2020 12 25)) ; Christmas 2020
(def date2 (t/date-time 2021 1 1)) ; New Year 2021

(t/after? date2 date1) ; true
(t/before? date1 date2) ; true
(t/equal? date1 date2) ; false
```

## Поглиблений вступ:
В історичному контексті, порівняння дат було проблемою, з якою програмісти стикалися від початку комп'ютерної ери. В Clojure ця задача вирішується за допомогою бібліотеки `clj-time`, що базується на Joda-Time для Java.

Але ви також можете використовувати інші методи або бібліотеки, такі як `java.time` від Java 8, якщо ви бажаєте роботу з вбудованими типами Java.

Три функції, що ми використали (`after?`, `before?`, `equal?`) внутрішньо перетворюють дати в мілісекунди (за допомогою `getMillis`), що дозволяє порівняти їх в числовій формі.

## Дивіться також:
1. Бібліотека clj-time: https://github.com/clj-time/clj-time
2. Joda-Time: http://www.joda.org/joda-time/
3. java.time: https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html