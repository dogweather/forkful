---
title:                "Розбір дати з рядка"
html_title:           "Clojure: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Що та чому?

Парсинг дати з рядка - це процес перетворення текстового рядка в об'єкт дати, який може бути оброблений і збережений у програмі. Програмісти використовують парсинг дати з рядка, щоб отримати зрозумілу дату з різних форматів рядків, таких як дати, дотримуючись кращих практик роботи з датами.

Як це зробити?

```Clojure
(ns clojure.date-parsing
  (:require [clojure.java-time :as t]))

;; Парсинг дати з рядка у форматі ISO-8601
(t/parse (java.time.format.DateTimeFormatter/ISO_DATE "2021-05-15")) 
;; Дату можна змінювати за допомогою різних шаблонів форматування
(t/format (java.time.format.DateTimeFormatter/BASIC_ISO_DATE "20210515"))

```

Глибока погрузка

- Історичний контекст: Існує багато різних форматів дат у різних культурах та для різних контекстів, що призводить до потреби універсального методу парсингу дати з рядка.
- Альтернативи: Інші мови програмування також мають свої методи парсингу дати з рядка, такі як `DateTime.Parse()` у C# та `Date.parse()` у JavaScript.
- Деталі реалізації: Парсинг дати з рядка в Clojure базується на Java-бібліотеці java.time, що надає зручні інструменти для роботи з датами та часом.

Дивіться також

- [Руководство Clojure з роботи з датами і часом](https://clojure.org/guides/time)
- [Документація по бібліотеці clojure.java-time](https://cljdoc.org/d/clojure.java-time/clojure.java-time/0.3.2/doc/readme)