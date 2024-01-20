---
title:                "Робота з json"
html_title:           "Clojure: Робота з json"
simple_title:         "Робота з json"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Що та чому?

Робота з JSON - це зручний спосіб для обміну даними між програмами. Програмісти використовують його, щоб легко передавати та отримувати дані, такі як рядки, числа та об'єкти, між різними додатками та системами.

## Як:

```Clojure
;; Створення JSON об'єкту
(def my-json {:name "John" :age 25})

;; Конвертація в рядок
(clojure.data.json/write-str my-json) ; {"name":"John", "age":25}

;; Зчитування з рядка
(clojure.data.json/read-str "{\"name\":\"Mary\", \"age\":30}") ; {:name "Mary", :age 30}
```

## Глибоке занурення:

JSON був створений для заміни складних форматів обміну даними, таких як XML. Особливістю JSON є його простота та універсальність, що робить його популярним у багатьох мов програмування. Альтернативою для роботи з JSON можуть бути бібліотеки, такі як Cheshire та Jsonista. Для роботи з JSON у Clojure використовується бібліотека clojure.data.json, яка надає зручні функції для роботи з цим форматом даних.