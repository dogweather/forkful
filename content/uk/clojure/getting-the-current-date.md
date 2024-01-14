---
title:    "Clojure: Отримання поточної дати"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Використання поточної дати є важливим аспектом багатьох програм, особливо у тих, що пов'язані з обробкою часу і дати. Отримання поточної дати може допомогти в різних сценаріях, від ведення журналу до розрахунків строків. У цій статті ми розберемося, як отримати поточну дату у мові Clojure.

## Як

```Clojure
(def current-date (java.util.Date.)) ; створює новий об'єкт типу Date

(defn today []
  (let [date (java.util.Date.)
        year (.getYear date)
        month (.getMonth date)
        day (.getDate date)]
    (println (str month "/" day "/" year))))

(today) ; виводить поточну дату у форматі "місяць/день/рік"
; наприклад: 06/19/2021
```

## Глибоке вивчення

Отримання поточної дати відбувається за допомогою стандартного Java класу "java.util.Date". У першому прикладі ми створюємо новий об'єкт цього класу, який включає у себе інформацію про поточну дату і час. Далі, ми використовуємо функцію today, яка використовує різні методи класу "java.util.Date" для отримання поточної дати і виводить цю інформацію у бажаному нами форматі. 

Також, для отримання поточної дати можна використати стандартну бібліотеку Clojure "clojure.java.time". Наприклад:

```Clojure
(require '[java-time :as t])

(def today (t/local-date)) ; створює об'єкт типу java.time.LocalDate з поточною датою

(t/format today "MM/dd/yyyy") ; виводить поточну дату у форматі "місяць/день/рік"
; наприклад: 06/19/2021
```

## Дивись також

- [Офіційна документація з Clojure](https://clojure.org/)
- [Курс Clojure на Codeacademy](https://www.codecademy.com/learn/learn-clojure)
- [Основи програмування на Clojure](https://clojure.org/guides/learn/syntax)