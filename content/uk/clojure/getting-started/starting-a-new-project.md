---
date: 2024-01-20 18:03:13.062269-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0423 Clojure, \u043D\u043E\u0432\u0438\u0439 \u043F\u0440\u043E\u0454\u043A\u0442\
  \ \u043D\u0430\u0439\u0447\u0430\u0441\u0442\u0456\u0448\u0435 \u0441\u0442\u0432\
  \u043E\u0440\u044E\u0454\u0442\u044C\u0441\u044F \u0437\u0430 \u0434\u043E\u043F\
  \u043E\u043C\u043E\u0433\u043E\u044E \u0456\u043D\u0441\u0442\u0440\u0443\u043C\u0435\
  \u043D\u0442\u0443 Leiningen. \u041E\u0441\u044C \u0431\u0430\u0437\u043E\u0432\u0456\
  \ \u043A\u0440\u043E\u043A\u0438."
lastmod: '2024-03-13T22:44:48.654610-06:00'
model: gpt-4-1106-preview
summary: "\u0423 Clojure, \u043D\u043E\u0432\u0438\u0439 \u043F\u0440\u043E\u0454\u043A\
  \u0442 \u043D\u0430\u0439\u0447\u0430\u0441\u0442\u0456\u0448\u0435 \u0441\u0442\
  \u0432\u043E\u0440\u044E\u0454\u0442\u044C\u0441\u044F \u0437\u0430 \u0434\u043E\
  \u043F\u043E\u043C\u043E\u0433\u043E\u044E \u0456\u043D\u0441\u0442\u0440\u0443\u043C\
  \u0435\u043D\u0442\u0443 Leiningen."
title: "\u041F\u043E\u0447\u0438\u043D\u0430\u0454\u043C\u043E \u043D\u043E\u0432\u0438\
  \u0439 \u043F\u0440\u043E\u0435\u043A\u0442"
weight: 1
---

## Як це зробити:
У Clojure, новий проєкт найчастіше створюється за допомогою інструменту Leiningen. Ось базові кроки:

```Clojure
;; Встановіть Leiningen, якщо ще не встановлений
;; https://leiningen.org/

;; Створіть новий проєкт
lein new app мій-новий-проєкт

;; Перейдіть в папку проєкту
cd мій-новий-проєкт

;; Запустіть REPL для ваших експериментів
lein repl

;; Вивід:
;; nREPL server started on port ...
;; REPL-y ...
;; Clojure ...
;; ...
;; => _

;; Запустіть ваш проєкт
lein run

;; Згенеруйте статичний сайт або документацію
lein marg
```

## Поглиблений Розбір:
Clojure, мова на базі Lisp, запущена у 2007 році Річем Хікі, має певні переваги для створення проєктів. Інструменти як Leiningen допомагають автоматизувати багато аспектів розробки: від створення проєкту до його збірки і запуску.

Альтернативою є Boot або новіші інструменти як tools.deps і CLI, які надають більше контролю і гнучкості розробникам.

У разі Leiningen, `project.clj` – це місце, де ви визначаєте залежності проєкту, параметри збірки та плагіни. Важливо розуміти цей файл, щоб налаштувати ваш проєкт під конкретні потреби.

## Дивіться Також:
- Офіційна документація Leiningen: [https://leiningen.org/](https://leiningen.org/)
- ClojureDocs, добірка прикладів використання Clojure: [https://clojuredocs.org/](https://clojuredocs.org/)
- Ресурси для вивчення Clojure: [https://clojure.org/community/resources](https://clojure.org/community/resources)
