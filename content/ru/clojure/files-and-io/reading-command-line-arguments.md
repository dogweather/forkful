---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:23.277350-07:00
description: "\u041A\u0430\u043A: \u0412 Clojure \u0430\u0440\u0433\u0443\u043C\u0435\
  \u043D\u0442\u044B \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\
  \u0442\u0440\u043E\u043A\u0438 \u043C\u043E\u0436\u043D\u043E \u043F\u043E\u043B\
  \u0443\u0447\u0438\u0442\u044C \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E\
  \ `*command-line-args*`. \u0412\u043E\u0442 \u043F\u0440\u043E\u0441\u0442\u043E\
  \u0439 \u043F\u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-03-13T22:44:44.378567-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Clojure \u0430\u0440\u0433\u0443\u043C\u0435\u043D\u0442\u044B \u043A\
  \u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\u0442\u0440\u043E\u043A\u0438\
  \ \u043C\u043E\u0436\u043D\u043E \u043F\u043E\u043B\u0443\u0447\u0438\u0442\u044C\
  \ \u0441 \u043F\u043E\u043C\u043E\u0449\u044C\u044E `*command-line-args*`."
title: "\u0427\u0442\u0435\u043D\u0438\u0435 \u0430\u0440\u0433\u0443\u043C\u0435\u043D\
  \u0442\u043E\u0432 \u043A\u043E\u043C\u0430\u043D\u0434\u043D\u043E\u0439 \u0441\
  \u0442\u0440\u043E\u043A\u0438"
weight: 23
---

## Как:
В Clojure аргументы командной строки можно получить с помощью `*command-line-args*`. Вот простой пример:

```clojure
;; Предположим, что этот код находится в файле `echo.clj`

(defn -main [& args]
  (println "Вы ввели:" args))

;; Для запуска: `clojure echo.clj arg1 arg2 arg3`
```

Пример вывода:

```
Вы ввели: (arg1 arg2 arg3)
```

Нужна обработка? Используйте функции коллекций Clojure.

```clojure
(defn -main [& args]
  (let [processed-args (mapv str/upper-case args)]
    (println "Преобразованные в верхний регистр:" processed-args)))

;; Теперь, выполняя `clojure echo.clj hello world`, вы получите следующий вывод:
```

Пример вывода:

```
Преобразованные в верхний регистр: ["HELLO" "WORLD"]
```

## Подробнее
`*command-line-args*` — это переменная в Clojure, установленная в последовательность аргументов, переданных скрипту. Она существует с ранних дней Clojure, что показывает, что Clojure относится к аргументам командной строки как к объектам первого класса.

Альтернативы? Механизмы Java для получения аргументов командной строки также работают в Clojure, благодаря взаимодействию. Но это более многословно.

Что касается деталей реализации, когда Clojure запускается, она анализирует аргументы и хранит их в `*command-line-args*`. Ваш скрипт затем может делать с ними что угодно — анализировать, игнорировать, преобразовывать, как захотите.

## Смотрите также
- Официальные инструменты Clojure CLI: https://clojure.org/guides/deps_and_cli
- Clojure с нуля: Скриптинг в командной строке: https://aphyr.com/posts/305-clojure-from-the-ground-up-command-line
- ClojureDocs о *command-line-args*: https://clojuredocs.org/clojure.core/*command-line-args*
