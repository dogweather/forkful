---
title:                "Перетворення рядка на великі літери"
date:                  2024-01-19
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Капіталізація рядків — це перетворення першої букви слова в верхній регістр. Програмісти використовують це для форматування тексту, наприклад, для назв статей чи для іменування сутностей у коді.

## How to: (Як це зробити:)
```Clojure
;; Простий приклад капіталізації
(defn capitalize-str [s]
  (string/capitalize s))

;; Використання
(println (capitalize-str "вітаємо у clojure!"))
;; Вихідні дані: Вітаємо у clojure!
```
```Clojure
;; Капіталізація кожного слова у рядку
(defn capitalize-words [s]
  (string/join " " (map string/capitalize (string/split s #" "))))

;; Використання
(println (capitalize-words "запрошуємо до svit clojure!"))
;; Вихідні дані: Запрошуємо До Svit Clojure!
```

## Deep Dive (Поглиблений аналіз)
Капіталізація була суттєвою для мов спілкування, а успішний перенос її у програмування залежав від стандартизації лінгвістичних правил у коді. У Clojure, капіталізацію можна реалізувати використовуючи стандартну бібліотеку `clojure.string`. Альтернативи включають самостійну реалізацію функцій для специфічних випадків.

Технічні деталі:
- `clojure.string/capitalize` перетворює перший символ рядку в uppercase.
- `map` застосовує функцію до кожного елементу колекції.
- `string/join` з'єднує елементи колекції в один рядок.
- `string/split` ділить рядок на колекцію рядків за допомогою регулярного виразу.

## See Also (Дивіться також)
- Clojure Official Documentation: [string functions](https://clojure.github.io/clojure/clojure.string-api.html)
- Clojure Style Guide: [naming conventions](https://guide.clojure.style/#naming)
- Clojure from the ground up: [strings](https://aphyr.com/posts/305-clojure-from-the-ground-up-strings)
