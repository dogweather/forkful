---
title:                "Clojure: Використання регулярних виразів."
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Тому, чому

Регулярні вирази є потужним інструментом для обробки інформації у коді. Вони дозволяють швидко та ефективно знаходити та замінювати текстові шаблони в рядках даних. Це дуже корисно для перевірки валідності введеного користувачем або для вибору певної інформації зі структурованих даних.

## Як використовувати

Щоб використовувати регулярні вирази в Clojure, вам потрібно імпортувати бібліотеку `clojure.string`, яка містить функцію `re-matches`. Вона приймає два аргументи: регулярний вираз та рядок, в якому треба його застосувати. Давайте подивимось на приклад:

```Clojure
(require '[clojure.string :as str])

(def regex #"hello")
(def str1 "Hello, world!")
(def str2 "Hi there!")

(str/re-matches regex str1)
;; "Hello"
(str/re-matches regex str2)
;; nil (не знайдено відповідності)
```

Цей приклад показує, як `re-matches` повертає першу збігаючу частину рядка або `nil`, якщо збігів не знайдено.

## Глибші відомості

Регулярні вирази в Clojure базуються на стандартних регулярних виразах Java, тому вони працюють з такими ж метасимволами, як `.*` (послідовність будь-яких символів) та `+` (один або більше входжень). Також є можливість застосовувати різні флаги, які змінюють поведінку виразу.

Для отримання більш докладної інформації про регулярні вирази в Clojure рекомендується прочитати [офіційну документацію](https://clojure.org/reference/java_interop#_regular_expressions) на цю тему.

## Дивіться також

- [Офіційна документація з регулярних виразів у Clojure](https://clojure.org/reference/java_interop#_regular_expressions)
- [Основи регулярних виразів в Clojure](https://stackabuse.com/regular-expressions-in-clojure/)
- [Застосування регулярних виразів при роботі зі строками у Clojure](https://openclassrooms.com/fr/courses/2189416-manipulation-de-chaines-de-caracteres-en-clojure/2297647-appliquer-des-expressions-regulieres)