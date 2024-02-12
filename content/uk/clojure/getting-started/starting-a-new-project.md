---
title:                "Починаємо новий проект"
aliases: - /uk/clojure/starting-a-new-project.md
date:                  2024-01-20T18:03:13.062269-07:00
model:                 gpt-4-1106-preview
simple_title:         "Починаємо новий проект"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що і чому?

Створення нового проєкту — це як відкривати чистий аркуш для ваших програмістських ідей. Програмісти роблять це, щоб трансформувати свої концепції в реальність, використовуючи код як інструменти для рішення задач.

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
