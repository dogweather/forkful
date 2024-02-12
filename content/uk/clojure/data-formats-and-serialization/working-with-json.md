---
title:                "Робота з JSON"
aliases:
- /uk/clojure/working-with-json/
date:                  2024-02-03T19:22:31.434947-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Робота з JSON (JavaScript Object Notation) у Clojure включає парсинг рядків JSON у структури даних Clojure (мапи, вектори) і навпаки. Це завдання є фундаментальним для веб-сервісів, API та додатків, яким потрібно комунікувати дані у структурованому, текстовому форматі, оскільки JSON універсально визнаний і підтримується в різних програмних середовищах.

## Як:
Clojure не містить вбудованих функцій для роботи з JSON, тому ви зазвичай будете використовувати сторонні бібліотеки. `cheshire` та `jsonista` є популярними виборами через їхню простоту використання та продуктивність.

### Використання Cheshire
Спершу додайте Cheshire до залежностей вашого проекту в `project.clj`:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

Щоб розібрати рядок JSON у мапу Clojure та конвертувати мапу в рядок JSON:

```clj
(require '[cheshire.core :as json])

;; Розбір рядка JSON у мапу Clojure
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Конвертація мапи Clojure у рядок JSON
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Використання Jsonista
Додайте Jsonista до вашого проекту `project.clj`:
```clj
[jsonista "0.3.2"]
```

Аналогічні дії з Jsonista:

```clj
(require '[jsonista.core :as j])

;; Розбір рядка JSON у Clojure
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Конвертація мапи Clojure у рядок JSON
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

У обох бібліотеках ви маєте можливість кодувати та декодувати більш складні структури даних, а також є додаткові функції та параметри, які дозволяють налаштувати процеси серіалізації та десеріалізації. Для більшості додатків продемонстрована функціональність забезпечує міцну основу для роботи з JSON у додатках Clojure.
