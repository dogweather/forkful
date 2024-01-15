---
title:                "Робота з csv"
html_title:           "Clojure: Робота з csv"
simple_title:         "Робота з csv"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## Чому

Готувати суші дімашніх м.Люди починають працювати з CSV для збереження, обробки та обміну даних. CSV є одним з найпоширеніших форматів для представлення табличних даних, тому знання роботи з ним є необхідним для багатьох програмістів та аналітиків даних.

## Як це зробити

```Clojure
;; Читання CSV файлу
(require '[clojure-csv.core :as csv])

(csv/read-csv "data.csv" :delimiter \,)

;; Запис CSV файлу
(csv/write-csv "new_data.csv" data)

;; Додавання рядка даних до CSV файлу
(csv/append-csv "data.csv" [1 "John Doe" "john@email.com"])
```

Зверніть увагу, що дані в CSV файлі представлені у вигляді списків Clojure, тому для роботи з ними можна використовувати звичні функції для списків, такі як `map`, `filter` та інші.

## Поглиблене дослідження

Для більш складних операцій обробки CSV даних існують бібліотеки, такі як `clojure.data.csv` та `clojure.data.csv`. Вони мають розширені можливості, такі як обробка різних типів даних, встановлення власних роздільників та інших параметрів.

Ще однією корисною функцією є зчитування даних з веб-ресурсів у форматі CSV за допомогою бібліотеки `clj-http`. Це дозволяє отримувати оновлені дані безпосередньо з Інтернету та опрацьовувати їх у програмі.

## Дивіться також

- [Clojure CSV бібліотека](https://github.com/didibus/csv)
- [clj-http бібліотека](https://github.com/dakrone/clj-http)
- [Clojure документація](https://clojure.org/)