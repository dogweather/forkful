---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:22.086969-07:00
description: "\u041A\u0430\u043A: \u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\
  \u0438\u043D\u0433 \u0432 Clojure \u2014 \u0431\u043B\u0430\u0433\u043E\u0434\u0430\
  \u0440\u044F \u0435\u0433\u043E \u0447\u0438\u0441\u0442\u043E\u043C\u0443 \u0441\
  \u0438\u043D\u0442\u0430\u043A\u0441\u0438\u0441\u0443 \u0438 \u0444\u0443\u043D\
  \u043A\u0446\u0438\u043E\u043D\u0430\u043B\u044C\u043D\u043E\u0439 \u043F\u0430\u0440\
  \u0430\u0434\u0438\u0433\u043C\u0435 \u2014 \u043C\u043E\u0436\u0435\u0442 \u0431\
  \u044B\u0442\u044C \u043D\u0435\u0432\u0435\u0440\u043E\u044F\u0442\u043D\u043E\
  \ \u043F\u0440\u043E\u0441\u0442\u044B\u043C. \u0414\u0430\u0432\u0430\u0439\u0442\
  \u0435 \u0440\u0430\u0441\u0441\u043C\u043E\u0442\u0440\u0438\u043C \u043E\u0431\
  \u044B\u0447\u043D\u044B\u0439\u2026"
lastmod: '2024-03-13T22:44:44.365579-06:00'
model: gpt-4-0125-preview
summary: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433 \u0432\
  \ Clojure \u2014 \u0431\u043B\u0430\u0433\u043E\u0434\u0430\u0440\u044F \u0435\u0433\
  \u043E \u0447\u0438\u0441\u0442\u043E\u043C\u0443 \u0441\u0438\u043D\u0442\u0430\
  \u043A\u0441\u0438\u0441\u0443 \u0438 \u0444\u0443\u043D\u043A\u0446\u0438\u043E\
  \u043D\u0430\u043B\u044C\u043D\u043E\u0439 \u043F\u0430\u0440\u0430\u0434\u0438\u0433\
  \u043C\u0435 \u2014 \u043C\u043E\u0436\u0435\u0442 \u0431\u044B\u0442\u044C \u043D\
  \u0435\u0432\u0435\u0440\u043E\u044F\u0442\u043D\u043E \u043F\u0440\u043E\u0441\u0442\
  \u044B\u043C."
title: "\u0420\u0435\u0444\u0430\u043A\u0442\u043E\u0440\u0438\u043D\u0433"
weight: 19
---

## Как:
Рефакторинг в Clojure — благодаря его чистому синтаксису и функциональной парадигме — может быть невероятно простым. Давайте рассмотрим обычный сценарий: итерация по коллекциям. Вы можете начать с цикла `for`, вот так:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

Вызов `(old-way)` даст нам 55, сумму от 1 до 10. Но, эй, мы можем рефакторить это, чтобы сделать более Clojure-образным:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

Эта рефакторинговая функция `(new-way)` использует макросы потоков для передачи диапазона непосредственно в `reduce`, уменьшая избыточность.

## Погружение
Искусство рефакторинга имеет корни в ранние дни разработки программного обеспечения, но действительно набрало обороты с выходом фундаментальной книги Мартина Фаулера "Рефакторинг: Улучшение проекта существующего кода", опубликованной в 1999 году. В Clojure рефакторинг часто опирается на принципы функционального программирования, отдавая предпочтение чистым функциям и неизменяемым структурам данных.

Альтернативы ручному рефакторингу в Clojure могут включать использование инструментов вроде Cursive, популярного плагина для IntelliJ IDEA, который предлагает автоматизированные рефакторинги, специфичные для Clojure. Также существует clj-refactor, пакет для Emacs для Clojure, предоставляющий набор функций для рефакторинга.

Особенной задачей при рефакторинге в Clojure является работа со состоянием и побочными эффектами в принципиально неизменяемой и свободной от побочных эффектов парадигме. Осторожное использование атомов, справок, агентов и транзиентов является ключевым в поддержании как производительности, так и корректности при рефакторингах.

## Смотрите также
- "Рефакторинг: Улучшение проекта существующего кода" Мартина Фаулера для основных концепций.
- [Документация Clojure](https://clojuredocs.org/) для конкретных примеров идиоматичного кода Clojure.
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) для автоматизации рефакторинга в Emacs.
- [Cursive](https://cursive-ide.com/) для пользователей IntelliJ, ищущих помощь в автоматизированном рефакторинге.
- [Рефакторинг с Ричем Хикки](https://www.infoq.com/presentations/Simple-Made-Easy/) - Доклад создателя Clojure, который, хотя и не о рефакторинге в частности, предоставляет представление о философии Clojure, которая может направлять эффективные решения по рефакторингу.
