---
title:                "Clojure: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Зчитування аргументів командного рядка є важливим навичкою для будь-якого програміста, оскільки це дозволяє створювати більш гнучкі та користувач-дружні інтерфейси для своїх програм.

## Як це зробити

Зчитування аргументів командного рядка можна зробити за допомогою функції *command-line-args* у Clojure. Давайте переглянемо код та результат виконання цієї функції.

```Clojure
(defn process-args [args]
    (println "Введені аргументи:" args))

(defn -main [& args]
    (let [cmd-args (command-line-args)]
        (process-args cmd-args)))
```

Вхідні аргументи можна передати в програму при запуску з командного рядка, наприклад так:

```shell
java -jar myprogram.jar arg1 arg2 arg3
```

При виконанні цього коду ми побачимо наступний результат:

```shell
Введені аргументи: (arg1 arg2 arg3)
```

Тепер ми можемо використати ці аргументи для подальшої обробки в нашій програмі.

## Глибше розглянемо

Функція *command-line-args* повертає список аргументів, переданих при запуску програми. Якщо ви хочете зчитувати аргументи з певних позицій, ви можете вказати це в параметрах:

```Clojure
(defn -main [& args]
    (let [cmd-args (command-line-args args 2)] ; зчитувати аргументи з позиції 2 та після неї
        (process-args cmd-args)))
```

Більше інформації про функцію *command-line-args* ви можете знайти в [офіційній документації Clojure](https://clojuredocs.org/clojure.core/command-line-args).

## Дивіться також

* [Офіційна документація Clojure](https://clojure.org/)
* [Стаття про обробку аргументів командного рядка в Clojure](https://dev.to/justintime/clojure-command-line-arguments-fa9)