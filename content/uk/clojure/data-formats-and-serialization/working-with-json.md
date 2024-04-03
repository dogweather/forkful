---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:31.434947-07:00
description: "\u042F\u043A: Clojure \u043D\u0435 \u043C\u0456\u0441\u0442\u0438\u0442\
  \u044C \u0432\u0431\u0443\u0434\u043E\u0432\u0430\u043D\u0438\u0445 \u0444\u0443\
  \u043D\u043A\u0446\u0456\u0439 \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\
  \u0438 \u0437 JSON, \u0442\u043E\u043C\u0443 \u0432\u0438 \u0437\u0430\u0437\u0432\
  \u0438\u0447\u0430\u0439 \u0431\u0443\u0434\u0435\u0442\u0435 \u0432\u0438\u043A\
  \u043E\u0440\u0438\u0441\u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0441\u0442\
  \u043E\u0440\u043E\u043D\u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\
  \u043A\u0438. `cheshire` \u0442\u0430 `jsonista` \u0454\u2026"
lastmod: '2024-03-13T22:44:48.689536-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u043D\u0435 \u043C\u0456\u0441\u0442\u0438\u0442\u044C \u0432\u0431\
  \u0443\u0434\u043E\u0432\u0430\u043D\u0438\u0445 \u0444\u0443\u043D\u043A\u0446\u0456\
  \u0439 \u0434\u043B\u044F \u0440\u043E\u0431\u043E\u0442\u0438 \u0437 JSON, \u0442\
  \u043E\u043C\u0443 \u0432\u0438 \u0437\u0430\u0437\u0432\u0438\u0447\u0430\u0439\
  \ \u0431\u0443\u0434\u0435\u0442\u0435 \u0432\u0438\u043A\u043E\u0440\u0438\u0441\
  \u0442\u043E\u0432\u0443\u0432\u0430\u0442\u0438 \u0441\u0442\u043E\u0440\u043E\u043D\
  \u043D\u0456 \u0431\u0456\u0431\u043B\u0456\u043E\u0442\u0435\u043A\u0438."
title: "\u0420\u043E\u0431\u043E\u0442\u0430 \u0437 JSON"
weight: 38
---

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
