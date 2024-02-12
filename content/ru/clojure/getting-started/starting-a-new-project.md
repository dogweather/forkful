---
title:                "Начало нового проекта"
aliases:
- ru/clojure/starting-a-new-project.md
date:                  2024-01-29T00:03:06.097031-07:00
model:                 gpt-4-0125-preview
simple_title:         "Начало нового проекта"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/starting-a-new-project.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?

Начало нового проекта означает настройку новой программной среды для вашего кода. Программисты делают это для старта разработки с чистого листа и организации своих мыслей в конкретный код.

## Как:

Для начала работы с проектом на Clojure мы будем использовать Leiningen, популярный инструмент сборки для Clojure:

``` Clojure
;; 1. Установите Leiningen, если ещё не сделали это (https://leiningen.org/)
;; 2. Создайте новый каркас проекта:
lein new app my-cool-app

;; 3. Перейдите в ваш новый проект:
cd my-cool-app

;; 4. Запустите REPL (цикл чтение-выполнение-вывод):
lein repl

;; Пример вывода:
;; nREPL server started on port 12345 on host 127.0.0.1 - nrepl://127.0.0.1:12345
;; REPL-y 0.4.4, nREPL 0.6.0
;; Clojure 1.10.1
;; Java 1.8.0_232
;;     Docs: (doc function-name-here)
;;           (find-doc "part-of-name-here")
;;   Source: (source function-name-here)
;;  Javadoc: (javadoc java-object-or-class-here)
;;     Exit: Control+D or (exit) or (quit)
;;  Results: Stored in vars *1, *2, *3, an exception in *e

;; 5. Создайте файл для вашего кода (src/my_cool_app/core.clj) и откройте его в вашем любимом текстовом редакторе.

;; 6. Напишите некоторый простой Clojure код:
(ns my-cool-app.core)

(defn say-hello []
  (println "Привет, мир Clojure!"))

;; 7. Запустите вашу функцию в REPL:
(my-cool-app.core/say-hello)

;; Пример вывода:
;; Привет, мир Clojure!
```

## Глубокое погружение

Проекты на Clojure часто начинаются с использования Leiningen или Boot для управления зависимостями, сборки и автоматизации задач. Leiningen существует с 2010 года и стал основным выбором для большинства Clojurists.

Существуют и альтернативные инструменты, такие как `deps.edn` и Clojure CLI инструменты, которые были введены Clojure/core для более простого управления зависимостями и конфигурацией проекта.

Clojure ценит неизменяемость и функциональное программирование. Правильное начало проекта подчеркивает чистоту управления состоянием и разделение обязанностей между функциями и пространствами имен.

Проекты обычно придерживаются стандартной структуры каталогов:
- `src/` для основного кода.
- `test/` для тестового кода.
- `resources/` для некодовых ресурсов.
- `project.clj` или `deps.edn` для управления зависимостями и конфигурациями.

Хорошей практикой является минимализм на старте. Добавляйте зависимости по мере необходимости, сохраняя ваш проект легким и управляемым.

## Смотрите также

- [Руководство по началу работы с Leiningen](https://leiningen.org/#getting-started)
- [Документация по Clojure](https://clojuredocs.org/)
- [Руководство по стилю Clojure](https://guide.clojure.style/)
- [Инструменты Clojure CLI](https://clojure.org/guides/getting_started)
- [Каталог инструментов Clojure](https://www.clojure-toolbox.com/)
