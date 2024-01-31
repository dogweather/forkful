---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:38:04.501112-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і Чому?
Перетворення рядка на нижній регістр означає заміну всіх великих літер на маленькі у текстовому рядку. Програмісти роблять це, аби уніфікувати дані для порівняння, сортування або для задоволення вимог системи.

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
