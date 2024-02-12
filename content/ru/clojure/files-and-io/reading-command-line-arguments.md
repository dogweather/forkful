---
title:                "Чтение аргументов командной строки"
aliases:
- /ru/clojure/reading-command-line-arguments.md
date:                  2024-01-29T00:01:23.277350-07:00
model:                 gpt-4-0125-preview
simple_title:         "Чтение аргументов командной строки"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/clojure/reading-command-line-arguments.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Чтение аргументов командной строки позволяет программе получать информацию непосредственно из команды терминала пользователя. Программисты делают это для настройки поведения программы без изменения самого кода.

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
