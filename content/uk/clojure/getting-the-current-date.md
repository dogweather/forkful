---
title:                "Clojure: Отримання поточної дати"
programming_language: "Clojure"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Не всім може здатися необхідним отримувати поточну дату у своїх програмах, проте це може бути дуже корисно. Наприклад, використання поточної дати допоможе у виведенні повідомлень, які мають відношення до конкретного дня або розрахунку періодів часу.

## Як

Для початку, потрібно імпортувати клас LocalDate з бібліотеки java.time, яка входить у стандартну бібліотеку Java. Потім, за допомогою функції now(), ми можемо отримати поточну дату. Наприклад:

```Clojure
(import '[java.time LocalDate])

(defn get-current-date []
  (let [current-date (.now LocalDate)]
    current-date))

(get-current-date) ;; 2021-10-22
```

Також, можна отримати поточну дату у різних форматах за допомогою функції format() і змінюючи параметри форматування. Наприклад:

```Clojure
(defn get-current-date-as-string []
  (let [current-date (.now LocalDate)]
    (.format current-date (java.time.format.DateTimeFormatter/ofPattern "dd-MM-yyyy"))))

(get-current-date-as-string) ;; "22-10-2021"
```

## Глибше

Є багато різних способів отримання поточної дати у Clojure, також існує багато різних форматів дати, які можна використовувати. Для отримання більш детальної інформації, можна ознайомитися з документацією на бібліотеку java.time.

## Дивись також

- [Документація на java.time бібліотеку](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Стаття про роботу з часом та датою в Clojure](https://clojuredocs.org/clojure.java-time/index.html)
- [Відео урок, що демонструє різні формати отримання поточної дати в Clojure](https://www.youtube.com/watch?v=ajX1rizbGJ4)