---
title:                "Обчислення дати в майбутньому або минулому"
html_title:           "Clojure: Обчислення дати в майбутньому або минулому"
simple_title:         "Обчислення дати в майбутньому або минулому"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому?
Обчислення дати у майбутньому або минулому - це знаходження точної дати, перейшовши певну кількість днів вперед чи назад від вказаної дати. Програмісти роблять це для розрахунку термінів, планування задач та обробки часових рамок.

## Як це зробити:
Тут ми використовуємо функцію `plus` від бібліотеки clj-time:

```Clojure
(ns example.core
  (:require [clj-time.core :as t]
            [clj-time.coerce :as c]
            [clj-time.periodic :as p]))

;; Розрахунок дати на 10 днів у майбутнє
(defn future-date [days]
  (t/plus (t/now) (t/days days)))

(future-date 10)
;; => #object[org.joda.time.DateTime 2914 "..."]
```

Цей код вираховує дату, яка буде через десять днів від сьогодні.

## Поглиблено:
1. Історичний контекст: Clojure - це діалект мови Lisp, і вона має багатий історичний фон обчислення дати. Мови Lisp вперше були створені в 1958 році.
2. Альтернативи: В Clojure є багато способів обчислення дати в майбутнє або минуле, з різними бібліотеками, як-от clj-time та java-time.
3. Імплементація: Функція `plus` в clj-time використовує Joda-Time бібліотеку, яка базується на стандартах ISO, що гарантує точність та широкий функціонал.

## Додаткові матеріали:
1. Офіційна документація clj-time: https://github.com/clj-time/clj-time
2. Інформація про обчислення дати в Clojure: https://practicalli.github.io/clojure/alternative-to-java-time.html
3. Руководство Joda-Time: http://joda-time.sourceforge.net/
4. Більше про Clojure: https://clojure.org/