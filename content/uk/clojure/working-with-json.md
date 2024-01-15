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

## Чому

JSON є одним з найпопулярніших форматів обміну даними в сучасному програмуванні. Розуміння того, як працювати з JSON, дозволить вам ефективно обробляти і обмінювати дані з іншими додатками та сервісами.

## Як

```Clojure
(require '[clojure.data.json :as json])
(def sample-json "{:name \"John\", :age 25, :hobbies [\"reading\", \"hiking\", \"cooking\"]}")
(json/read-str sample-json)
```

В результаті ви отримаєте наступний вихід:

```
{"name" "John", "age" 25, "hobbies" ["reading" "hiking" "cooking"]}
```

На початку ми використовуємо функцію `(require ...)` для підключення модуля `clojure.data.json`. Потім ми створюємо змінну `sample-json` з рядком у форматі Clojure. Нарешті, ми використовуємо функцію `json/read-str` для перетворення рядка у структуру даних Clojure.

## Глибше вдивимось

Хоча в розглянутому прикладі ми просто перетворили рядок JSON у структуру даних Clojure, є інші корисні функції для роботи з JSON. Наприклад, `json/read` дозволяє читати дані з файлу, а `json/write` - записувати дані у файл у форматі JSON.

Також є можливість обробляти дані з більш складною структурою, такою як вкладені об'єкти та масиви. Наприклад:

```clojure
(def sample-json "{:employees [{:name \"John\", :salary 50000}, {:name \"Mary\", :salary 60000}]}")
(json/read-str sample-json)
```

В результаті ми отримаємо структуру даних з двома елементами, кожен з яких містить ім'я працівника та його зарплату. Також, ви можете використовувати функцію `json/write-str` для перетворення структури даних у рядок JSON для подальшого обміну з іншими додатками.

## Дивись також

Для отримання додаткової інформації про роботу з JSON у Clojure, рекомендуємо переглянути наступні ресурси:

- [Clojure JSON бібліотека](https://github.com/clojure/data.json)
- [Офіційна документація по Clojure](https://clojure.org/)
- [Онлайн курс "Clojure for the Brave and True"](https://www.braveclojure.com/)