---
title:                "Робота з рядками великої літери"
html_title:           "Clojure: Робота з рядками великої літери"
simple_title:         "Робота з рядками великої літери"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Підпередження: Люди можуть хотіти використовувати різні великі літери для різних цілей, наприклад для показу назви країни у великій літері, для оформлення заголовків або для виділення важливих слів. Також, використовування великих літер може полегшити читання і розуміння коду.

## Як це зробити

```Clojure
(defn capitalize-string [str]
  (clojure.string/capitalize str))

;; Приклад виклику функції
(capitalize-string "приклад тексту") 
;; Результат: "Приклад тексту"
```

```Clojure
(def my-string "цей текст потрібно зробити Великим")

;; Використання бібліотеки clojure.string
(require '[clojure.string :as str])

;; Використання функції clojure.string/upper-case
(str/upper-case my-string)
;; Результат: "ЦЕЙ ТЕКСТ ПОТРІБНО ЗРОБИТИ ВЕЛИКИМ"
```

## Глибокий погляд

Якщо потрібно зробити всі літери у рядку великими, можна також використовувати функцію "upcase" з бібліотеки "clojure.string". Також, можна створити свою власну функцію capitalize, яка буде перетворювати першу літеру у рядку на велику. Наприклад:

```Clojure
(defn capitalize [str]
  (str (clojure.string/upper-case (subs str 0 1)) (subs str 1)))

;; Приклад виклику функції
(capitalize "рандомний текст") 
;; Результат: "Рандомний текст"
```

## Дивіться також

- Документація бібліотеки clojure.string: https://clojuredocs.org/clojure.string
- Огляд функцій з рядками у Clojure: https://www.baeldung.com/clojure/string-functions