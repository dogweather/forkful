---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Інтерполяція рядків - це процес вставки змінних або виразів безпосередньо в текстовий рядок. Це корисно для створення динамічного виводу та зменшує необхідність складних рядкових операцій.

## Як це робити:
Основний спосіб інтерполяції рядків в Clojure - за допомогою бібліотеки `clojure.string/replace`.
```Clojure
(require '[clojure.string :as str])

(def my-var "world")

(str/replace "Hello, %s!" "%s" my-var)
```
Вивід:
```Clojure
"Hello, world!"
```
## Поглиблений аналіз
Слід зазначити, що Clojure відрізняється від інших мов, які мають вбудовані функції для інтерполяції рядків. В Clojure ви використовуєте `clojure.string/replace` для цієї цілі.

Інший варіант - це використання `format`, яке надає більше можливостей, але менш просте у використанні.
```Clojure
(format "Hello, %s!" my-var)
```
Важливо пам'ятати, що рядкова інтерполяція означає, що рядок розглядається як шаблон, а не як літеральний рядок. Це може привести до уразливостей безпеки, якщо не використовується розумно.

## Дивіться також
1. [Clojure - Strings](https://www.tutorialspoint.com/clojure/clojure_strings.htm) - Більше про рядки в Clojure.
2. [String Interpolation in Clojure](https://stackoverflow.com/questions/3787908/string-interpolation-in-clojure) - Дискусія на StackOverflow про інтерполяцію рядків в Clojure.
3. [Clojure by Example](https://kimh.github.io/clojure-by-example/#about) - Вивчіть Clojure за допомогою інтерактивних прикладів.