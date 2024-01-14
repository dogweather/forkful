---
title:    "Clojure: Отримання поточної дати"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Чому

Здорові букви! Сьогодні ми поговоримо про те, як отримати поточну дату в програмі на Clojure. І ви можете запитати: "Навіщо це мені потрібно?". Але насправді, це дуже корисна навичка для будь-якого програміста. Знання поточної дати може бути корисним для створення логувань, планування подій або навіть просто для відображення поточного часу на додатку.

## Як

Найпростіший спосіб отримати поточну дату в Clojure - це використовувати вбудовану функцію `(org.joda.time.DateTime/now)`, яка повертає поточний час у форматі DateTime. Давайте подивимось на кодовий приклад:

```Clojure
(org.joda.time.DateTime/now)
```

Також ми можемо додати деякі параметри для отримання поточного часу з конкретної часової зони. Наприклад, якщо нам потрібна дата та час у Київській часовій зоні, ми можемо використати функцію `(org.joda.time.DateTimeZone/forID "Europe/Kiev")`. Ось приклад коду:

```Clojure
(def kiev-time (org.joda.time.DateTime/now (org.joda.time.DateTimeZone/forID "Europe/Kiev")))
```

Крім того, для відображення дати або часу у зрозумілому людям форматі, ми можемо використати функцію `(.toString)` для перетворення дати у рядок. Ось приклад:

```Clojure
(def kiev-time-str (.toString kiev-time))
```

Результат виглядатиме приблизно так: "2019-06-10T15:30:00.000+03:00".

## Поглиблене дослідження

Clojure використовує бібліотеку Joda для роботи з датами і часом, тому більш детальний огляд можна знайти на [офіційному сайті Joda](https://www.joda.org/joda-time/).

Крім того, існують інші способи отримання поточної дати в Clojure, наприклад, використання Java класів. Але зробити це за допомогою `org.joda.time.DateTime` є досить просто та елегантно.

## Дивіться також

- [Differences between Joda-Time and Java Time API](https://www.baeldung.com/joda-time-java-8-differences)
- [Clojure API Docs for org.joda.time.DateTime](https://clojure.github.io/clojure/clojure.java-time-api-docs/org.joda.time.DateTime.html)