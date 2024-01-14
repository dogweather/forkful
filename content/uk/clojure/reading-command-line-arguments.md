---
title:    "Clojure: Читання аргументів командного рядка"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Читання аргументів командного рядка може бути корисним для розробників, які хочуть створити програми з можливістю взаємодії з користувачем. Це дозволяє передавати дані або налаштування програми безпосередньо через командну строку.

## Як

```Clojure
(defn read-cmd-args []
    "Функція для зчитування аргументів командного рядка"
    (let [cmd-args (rest *command-line-args*)]
        (println "Аргументи командного рядка:" cmd-args)))

(read-cmd-args)
```

Приклад входу: `lein run arg1 arg2`

Вихід: `Аргументи командного рядка: (arg1 arg2)`

## Глибоке занурення

Для більш складних програм, можливо, буде потрібно зчитувати аргументи командного рядка як числа або булеві значення. Для цього можна використовувати функцію `parse-int` або `parse-bool` з програми `clojure.edn` для перетворення аргументів у відповідні типи даних.

```Clojure
(require '[clojure.edn :as edn])

(defn read-cmd-args []
    "Функція для зчитування аргументів командного рядка"
    (let [cmd-args (rest *command-line-args*)
          int-arg-1 (edn/parse-int (first cmd-args))
          bool-arg-2 (if (= (second cmd-args) "true") (edn/parse-bool "true") (edn/parse-bool "false"))]
        (println "Числовий аргумент:" int-arg-1)
        (println "Булевий аргумент:" bool-arg-2)))

(read-cmd-args)
```

Приклад входу: `lein run 10 true`

Вихід: `Числовий аргумент: 10` та `Булевий аргумент: true`

## Дивіться також

- [The Clojure Programming Language](https://clojure.org/)
- [Command-Line Arguments in Clojure - Baeldung](https://www.baeldung.com/clojure-command-line-arguments)
- [Working with Command Line Arguments in Clojure - Mindful Dev Mag](https://mindfuldevmag.com/clojure/working-with-command-line-arguments-in-clojure/)