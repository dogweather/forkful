---
title:                "Clojure: Перетворення рядка на нижній регістр"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Багато разів у програмуванні знадобиться конвертувати рядок у нижній регістр. Наприклад, для порівняння рядків або для коректного відображення тексту. У цій статті ми розглянемо, як це зробити в мові Clojure.

## Як це зробити

Щоб перетворити рядок у нижній регістр в Clojure, нам знадобиться використати функцію `clojure.string/lower-case`. Ця функція приймає один аргумент - рядок, який потрібно конвертувати, і повертає новий рядок з нижнім регістром символів.

```Clojure
(clojure.string/lower-case "HELLO") ; виведе "hello"
(clojure.string/lower-case "Good Morning") ; виведе "good morning"
```

Давайте розглянемо ще кілька прикладів використання цієї функції:

```Clojure
(clojure.string/lower-case "Привіт") ; виведе "привіт"
(clojure.string/lower-case "123 АБВ") ; виведе "123 абв"
```

Ця функція також працює зі списками рядків:

```Clojure
(clojure.string/join " " (map clojure.string/lower-case ["Привіт" "Світ"])) ; виведе "привіт світ"
```

## Поглиблене вивчення

Щоб краще зрозуміти, як працює конвертування рядка в нижній регістр, давайте поглянемо на приклад реалізації функції `clojure.string/lower-case`:

```Clojure
(defn lower-case
  "Перетворює рядок в нижній регістр."
  [s]
  (reduce #(str %1 (Character/toLowerCase %2)) "" s))
```

Ця функція використовує функцію `reduce`, щоб ітеративно пройти по всім символам рядка і вибрати нижній регістр для кожного з них за допомогою функції `Character/toLowerCase`.

Також варто зазначити, що в реальних програмах для роботи зі строками рекомендується використовувати бібліотеку `core.string` замість `clojure.string`. Вона має більш продуману та оптимізовану реалізацію функції `lower-case`.

## Дивись також

- [Документація по функції `clojure.string/lower-case`](https://clojuredocs.org/clojure.string/lower-case)
- [Документація по бібліотеці core.string](https://clojuredocs.org/clojure.core/string)