---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:03:06.097031-07:00
description: "\u041A\u0430\u043A: \u0414\u043B\u044F \u043D\u0430\u0447\u0430\u043B\
  \u0430 \u0440\u0430\u0431\u043E\u0442\u044B \u0441 \u043F\u0440\u043E\u0435\u043A\
  \u0442\u043E\u043C \u043D\u0430 Clojure \u043C\u044B \u0431\u0443\u0434\u0435\u043C\
  \ \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C Leiningen,\
  \ \u043F\u043E\u043F\u0443\u043B\u044F\u0440\u043D\u044B\u0439 \u0438\u043D\u0441\
  \u0442\u0440\u0443\u043C\u0435\u043D\u0442 \u0441\u0431\u043E\u0440\u043A\u0438\
  \ \u0434\u043B\u044F Clojure."
lastmod: '2024-03-13T22:44:44.351097-06:00'
model: gpt-4-0125-preview
summary: "\u0414\u043B\u044F \u043D\u0430\u0447\u0430\u043B\u0430 \u0440\u0430\u0431\
  \u043E\u0442\u044B \u0441 \u043F\u0440\u043E\u0435\u043A\u0442\u043E\u043C \u043D\
  \u0430 Clojure \u043C\u044B \u0431\u0443\u0434\u0435\u043C \u0438\u0441\u043F\u043E\
  \u043B\u044C\u0437\u043E\u0432\u0430\u0442\u044C Leiningen, \u043F\u043E\u043F\u0443\
  \u043B\u044F\u0440\u043D\u044B\u0439 \u0438\u043D\u0441\u0442\u0440\u0443\u043C\u0435\
  \u043D\u0442 \u0441\u0431\u043E\u0440\u043A\u0438 \u0434\u043B\u044F Clojure."
title: "\u041D\u0430\u0447\u0430\u043B\u043E \u043D\u043E\u0432\u043E\u0433\u043E\
  \ \u043F\u0440\u043E\u0435\u043A\u0442\u0430"
weight: 1
---

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
