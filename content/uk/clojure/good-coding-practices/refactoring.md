---
date: 2024-01-26 01:18:10.660019-07:00
description: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 -\
  \ \u0446\u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0440\u0435\u0441\u0442\u0440\
  \u0443\u043A\u0442\u0443\u0440\u0438\u0437\u0430\u0446\u0456\u0457 \u0456\u0441\u043D\
  \u0443\u044E\u0447\u043E\u0433\u043E \u043A\u043E\u043C\u043F'\u044E\u0442\u0435\
  \u0440\u043D\u043E\u0433\u043E \u043A\u043E\u0434\u0443 \u0431\u0435\u0437 \u0437\
  \u043C\u0456\u043D\u0438 \u0439\u043E\u0433\u043E \u0437\u043E\u0432\u043D\u0456\
  \u0448\u043D\u044C\u043E\u0457 \u043F\u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438\
  , \u043D\u0430\u0446\u0456\u043B\u0435\u043D\u0438\u0439 \u043D\u0430 \u043F\u043E\
  \u043A\u0440\u0430\u0449\u0435\u043D\u043D\u044F \u043D\u0435\u0444\u0443\u043D\u043A\
  \u0446\u0456\u043E\u043D\u0430\u043B\u044C\u043D\u0438\u0445\u2026"
lastmod: '2024-03-13T22:44:48.667548-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 - \u0446\
  \u0435 \u043F\u0440\u043E\u0446\u0435\u0441 \u0440\u0435\u0441\u0442\u0440\u0443\
  \u043A\u0442\u0443\u0440\u0438\u0437\u0430\u0446\u0456\u0457 \u0456\u0441\u043D\u0443\
  \u044E\u0447\u043E\u0433\u043E \u043A\u043E\u043C\u043F'\u044E\u0442\u0435\u0440\
  \u043D\u043E\u0433\u043E \u043A\u043E\u0434\u0443 \u0431\u0435\u0437 \u0437\u043C\
  \u0456\u043D\u0438 \u0439\u043E\u0433\u043E \u0437\u043E\u0432\u043D\u0456\u0448\
  \u043D\u044C\u043E\u0457 \u043F\u043E\u0432\u0435\u0434\u0456\u043D\u043A\u0438\
  , \u043D\u0430\u0446\u0456\u043B\u0435\u043D\u0438\u0439 \u043D\u0430 \u043F\u043E\
  \u043A\u0440\u0430\u0449\u0435\u043D\u043D\u044F \u043D\u0435\u0444\u0443\u043D\u043A\
  \u0446\u0456\u043E\u043D\u0430\u043B\u044C\u043D\u0438\u0445\u2026"
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
---

{{< edit_this_page >}}

## Що та Чому?

Рефакторинг - це процес реструктуризації існуючого комп'ютерного коду без зміни його зовнішньої поведінки, націлений на покращення нефункціональних атрибутів. Програмісти проводять рефакторинг, щоб зробити їхній код чистішим, ефективнішим і легшим для підтримки, ефективно підвищуючи зрозумілість і зменшуючи складність їхнього програмного забезпечення.

## Як робити:

Рефакторинг в Clojure—завдяки його чистому синтаксису та функціональному парадигму—може бути надзвичайно прямолінійним. Спробуємо розібратися на поширеному прикладі: ітерація по колекціях. Ви могли почати з циклу `for`, ось так:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Виклик `(old-way)` дасть нам 55, суму від 1 до 10. Але, гей, ми можемо рефакторити це, щоб було більш Clojure-еским:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Ця рефакторена функція `(new-way)` використовує макроси потоку для прямої передачі діапазону в `reduce`, зрізаючи зайве.

## Поглиблений Розгляд

Мистецтво рефакторингу має свої корені в ранні дні розробки програмного забезпечення, але дійсно набуло популярності з книгою Мартіна Фаулера "Refactoring: Improving the Design of Existing Code", яка була опублікована в 1999 році. В Clojure, рефакторинг часто спирається на принципи функціонального програмування, віддаючи перевагу чистим функціям і незмінним структурам даних.

Альтернативами ручному рефакторингу в Clojure можуть бути такі інструменти, як Cursive, популярний плагін для IntelliJ IDEA, який пропонує автоматизовані рефакторинги спеціально для Clojure. Також є clj-refactor, пакет для Emacs для Clojure, що надає набір функцій для рефакторингу.

Однією з особливих викликів рефакторингу в Clojure є робота зі станом та побічними ефектами в принципово незмінному і вільному від побічних ефектів парадигмі. Ретельне використання атомів, refs, агентів та транзієнтів є вирішальним для збереження як продуктивності, так і правильності під час рефакторингу.

## Див. також

- "Refactoring: Improving the Design of Existing Code" Мартіна Фаулера для основоположних концептів.
- [Clojure Docs](https://clojuredocs.org/) для конкретних прикладів ідіоматичного Clojure коду.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) для автоматизації рефакторингу в Emacs.
- [Cursive](https://cursive-ide.com/) для користувачів IntelliJ, які шукають автоматизовану допомогу в рефакторингу.
- [Рефакторинг з Річем Гіккі](https://www.infoq.com/presentations/Simple-Made-Easy/) - Доповідь творця Clojure, яка, хоча і не про рефакторинг власне, надає розуміння філософії Clojure, яка може керувати ефективними рішеннями рефакторингу.
