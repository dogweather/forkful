---
title:                "Пошук та заміна тексту"
html_title:           "Clojure: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Чому

Пошук та заміна тексту є важливими інструментами для редагування та покращення коду. Використовуючи ці методи, можна швидко знайти та замінити певний текст у файлі або на веб-сторінці без необхідності вручну шукати та змінювати кожне входження.

## Як це зробити

```Clojure
;; Основна форма для пошуку та заміни тексту у строках
(clojure.string/replace "Hello, world!" "world!" "clojure!")

;; Виведе: "Hello, clojure!"

;; Заміна усіх входжень певного слова
(clojure.string/replace "I love apples, apples are my favorite fruit." "apples" "oranges")

;; Виведе: "I love oranges, oranges are my favorite fruit."

;; Заміна за допомогою регулярних виразів
(clojure.string/replace "The code is 12345" #"\d+" "67890")

;; Виведе: "The code is 67890"
```

## Глибока розробка

Функція `replace` з модуля `clojure.string` використовується для пошуку та заміни тексту. Ця функція може приймати як прості текстові значення, так і регулярні вирази для пошуку. Для заміни усіх входжень тексту використовується функція `replace-first`, а для заміни лише першого входження - функція `replace-recursive`.

## Додаткове дослідження

Існує також багато інших функцій та бібліотек для пошуку та заміни тексту в Clojure, таких як `str/replace-first`, `clojure.contrib.string-replace`, `cljrepl`, тощо. Для більш детального огляду можна ознайомитися з наступними ресурсами:

* [Clojure документація по `replace`](https://clojuredocs.org/clojure.string/replace)
* [`clojure.string` уроки від Clojure for the Brave and True](https://www.braveclojure.com/strings/)
* [Clojure чат-група на Slack](https://clojurians.slack.com/)