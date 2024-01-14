---
title:    "Clojure: Порівняння двох дат"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Найперше, перед тим як глибше заглиблюватися у порівняння двох дат, важливо зрозуміти, чому це може бути корисно. Один з найбільш поширених випадків - це порівняння двох дат, щоб визначити, яка з них була пізніше або раніше. Це особливо корисно при роботі з даними, де потрібно відстежувати часові проміжки.

## Як це зробити

Для порівняння двох дат у Clojure ми використовуємо функцію `compare`, яка порівнює два об'єкти дати і повертає `-1`, якщо перша дата є ранішою, `1`, якщо друга дата є ранішою, або `0`, якщо дати рівні. Приклад коду наведено нижче:

```Clojure
(def d1 (java.util.Date. 2021 6 1))
(def d2 (java.util.Date. 2021 6 15))

(println (compare d1 d2)) ; виведе -1
```

У цьому прикладі ми порівнюємо дві дати: перша - 1 червня 2021 року, а друга - 15 червня 2021 року. Використовуючи функцію `compare`, ми бачимо, що перша дата є ранішою за другу.

## Глибше заглиблюємось

У Clojure дати представлені об'єктами класу `java.util.Date`, але також є більш універсальний тип дати - `java.time.LocalDate`. Цей тип має багато корисних методів для порівняння дат, як, наприклад, `isBefore`, `isEqual`, `isAfter`. Приклад використання:

```Clojure
(def d1 (java.time.LocalDate/of 2021 6 1))
(def d2 (java.time.LocalDate/of 2021 6 15))

(println (d1.isBefore d2)) ; виведе true
```

У цьому прикладі ми використовуємо метод `isBefore`, який повертає `true`, якщо перша дата є ранішою за другу.

## Дивись також

- [Documentation on comparing dates in Clojure](https://clojuredocs.org/clojure.core/compare)
- [Java documentation on Date class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Date.html)
- [Java documentation on LocalDate class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/time/LocalDate.html)