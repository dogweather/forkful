---
date: 2024-01-20 17:38:04.501112-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Clojure \u0440\u043E\u0431\u0438\u0442\u044C \u0446\u0435 \u043F\u0440\u043E\u0441\
  \u0442\u043E \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E\
  \ \u0444\u0443\u043D\u043A\u0446\u0456\u0457 `clojure.string/lower-case`. \u041E\
  \u0441\u044C \u044F\u043A."
lastmod: '2024-03-13T22:44:48.631194-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u0440\u043E\u0431\u0438\u0442\u044C \u0446\u0435 \u043F\u0440\u043E\
  \u0441\u0442\u043E \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\
  \u044E \u0444\u0443\u043D\u043A\u0446\u0456\u0457 `clojure.string/lower-case`."
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

## Як це зробити:
Clojure робить це просто за допомогою функції `clojure.string/lower-case`. Ось як:

```clojure
(require '[clojure.string :as str])

;; Перетворення рядка на нижній регістр
(str/lower-case "Hello, World!")
;; Вивід: "hello, world!"
```

## Поглиблений Розбір:
У більшості мов програмування є функція для переведення тексту в нижній регістр. У Clojure вона з'явилась завдяки систематичній потребі зручної обробки тексту. Альтернативою є написання власної функції з використанням ітерації по символах рядка та їх зміни за допомогою Unicode таблиць. За лаштунками, `str/lower-case` може використовувати саме такий підхід, залежно від того, як це реалізоване в JVM, на якому виконується Clojure.

## Дивись Також:
- [ClojureDocs - clojure.string/lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [Oracle JavaDocs - Character](https://docs.oracle.com/javase/7/docs/api/java/lang/Character.html)
