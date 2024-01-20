---
title:                "Перетворення рядка в верхній регістр"
html_title:           "Clojure: Перетворення рядка в верхній регістр"
simple_title:         "Перетворення рядка в верхній регістр"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це та навіщо?

Ми маємо на увазі зробити першу букву у рядках великою, тобто "капіталізувати" їх. Програмісти роблять це для більшої чіткості в тексті: заголовках, іменах чи реченнях.

## Як це робити:

Clojure дає нам чудову функцію `clojure.string/capitalize` щоб саме це і реалізувати. Використовуйте її так:

```clojure
(require '[clojure.string :as str])

(defn capitalize-string [s]
  (str/capitalize s))

(prn (capitalize-string "ukrainian text"))
```

Виведе:

```clojure
"Ukrainian text"
```

## Занурення у деталі:

1. **Історичний контекст**: Якщо ви знайомі з іншими мовами (наприклад, JavaScript), ви можете знати, що ця функція є поширеною. Clojure також має її, але з додатковою родзинкою: вона не тільки робить першу букву великою, але і решту рядка перетворює в нижній регістр. 

2. **Альтернативи**: Якщо вам потрібно лише зробити першу букву великою (функція JS-сучасно), ви можете створити свою власну функцію:

```clojure
(defn capitalize-first [s]
  (str/replace-first s (str/first s) (str/upper-case (str/first s))))
```

3. **Деталі реалізації**: Якщо ви заглибитеся у вихідний код `clojure.string/capitalize`, ви побачите, що він використовує Java для роботи з рядками. Це надає йому високу продуктивність і надійність.

## Більше інформації:

1. [Офіційна документація Clojure](https://clojure.org/api/api)
2. [Про бібліотеку clojure.string](https://clojuredocs.org/clojure.string)
3. [Інтерактивне навчання Clojure](https://www.braveclojure.com/clojure-for-the-brave-and-true/)