---
title:                "Робота з JSON"
html_title:           "Arduino: Робота з JSON"
simple_title:         "Робота з JSON"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
JSON (JavaScript Object Notation) - текстовий формат для зберігання і передачі даних. Використовується у програмуванні, бо дозволяє легко обмінюватись даними між сервером та клієнтом або різними системами.

## Як це зробити:
В Clojure для роботи з JSON можна використати бібліотеку `cheshire`. Ось як просто зчитати JSON з рядка і преобразувати його у мапу Clojure:

```Clojure
(require '[cheshire.core :as json])

;; Читання JSON з рядка:
(def json-str "{\"name\":\"Jon\", \"age\":30}")
(def data (json/parse-string json-str))

;; Перетворення Clojure мапи в JSON рядок:
(def clojure-map {:name "Jon" :age 30})
(def json-output (json/generate-string clojure-map))

;; Вивід:
(println data)         ; Виводить: {"name" "Jon", "age" 30}
(println json-output)  ; Виводить: {"name":"Jon","age":30}
```

Результатом буде дві мапи: одна зроблена з JSON, інша - з Clojure коду.

## Пірнаємо глибше:
JSON став стандартом у веб-розробці після його згадування Дугласом Крокфордом на початку 2000-х. Альтернативами JSON можуть бути XML або YAML, але JSON частіше вибирають через простоту і швидкість.

Clojure реалізовує роботу з JSON за допомогою бібліотек, як-от `cheshire`, що використовує Java бібліотеку `Jackson` під капотом. Також існує `Clojure.data.json`, яка є більш легковесною але з меншим функціоналом.

## Дивіться також:
- Clojure Cheatsheet: [https://clojure.org/api/cheatsheet](https://clojure.org/api/cheatsheet)
- Cheshire GitHub Page: [https://github.com/dakrone/cheshire](https://github.com/dakrone/cheshire)
- JSON офіційний сайт: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)