---
title:                "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

##Чому

Найчастіше, у програмуванні потрібно порівнювати дати для виконання різних умов або функцій в залежності від часу. В цьому блозі ми розглянемо, яким чином порівнювати дати в Clojure та навіщо це потрібно.

##Як це зробити

```Clojure
(ns date-comparison.core
  (:require [clj-time.core :as t]))

(def start-date (t/date-time 2020 1 1))
(def end-date (t/date-time 2020 12 31))

(println (t/plus start-date (t/days 7))) ;; Output: 2020-01-08T00:00:00.000+00:00
(println (t/greater? start-date end-date)) ;; Output: false
```

В цьому прикладі ми визначили дві змінні - start-date та end-date - з об'єктами дат, і використовуємо функції з тим самими назвами для порівняння дат та обчислення нової дати на основі початкової.

##Глибші дослідження

При порівнянні дат важливо враховувати часові зони та жодних різниць у точності. Clojure надає багато функцій для роботи з датами та часом, включаючи можливість конвертації, форматування та обчислення різниць між датами.

Одна з корисних функцій - t/interval - дозволяє обчислювати різницю між двома датами у певних одиницях, наприклад роки, місяці чи дні. Крім того, для більш складних обчислень можна використовувати Clojure.java-time бібліотеку, яка надає доступ до Java 8 API для роботи з датами та часом.

##Дивись також

- [Clojure документація по датам та часу](https://clojure.github.io/java-time/)
- [Дивні випадки у Clojure з датами](http://thinkrelevance.com/blog/2013/06/04/strange-loops-clojure-date-time)

##Подібні статті

- [Робота з датами та часом в Clojure](https://amsterdam.luminis.eu/2016/10/24/working-with-dates-and-times-in-clojure/) 
- [Робота зі змінними датами в Clojure](https://technologyconversations.com/2014/02/14/java-8-date-and-time-clojure/)