---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що та чому?
Стрічки в Clojure можна об'єднати, використовуючи функцію `str`. Це може бути корисним, коли ви бажаєте створити нову строку з ряду інших рядків або значень.

## Як це робити:
Об'яснимо, як можна об'єднати рядки в Clojure, за допомогою декількох прикладових кодів:
```Clojure
(str "Привіт, " "Світ") ; => "Привіт, Світ"
```
Ви також можете передавати у функцію `str` багато рядків:
```Clojure
(str "Clojure" " - " "це" " приємно!") ; => "Clojure - це приємно!"
```
Або ви можете об'єднати рядок з іншими типами даних:
```Clojure
(str "Рік заснування Clojure - " 2007) ; => "Рік заснування Clojure - 2007"
```

## На глибину:
Clojure використовує Java String для маніпуляцій з рядками. Функція `str` в Clojure просто об'єднує рядки або перетворює значення в рядок і об'єднує їх. Це витік було взято з Java, де основний спосіб об'єднання рядків - це через оператор `+`. Однак, в Clojure немає операторів, тому ми використовуємо функцію `str`.

Як альтернативу розглянемо використання функції `format`, яка працює подібно до `sprintf` у С:
```Clojure
(format "Привіт, %s" "Світ") ; => "Привіт, Світ"
```

## Дивіться також:
1. Clojure for the Brave and True: [Strings](http://www.braveclojure.com/clojure-for-the-brave-and-true#Strings)