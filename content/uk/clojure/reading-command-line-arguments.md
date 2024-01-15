---
title:                "Читання аргументів командного рядка."
html_title:           "Clojure: Читання аргументів командного рядка."
simple_title:         "Читання аргументів командного рядка."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Чому

Частина роботи програміста полягає в інтерактивному взаємодії зі своїм кодом. Командний рядок - це потужний інструмент, який дозволяє керувати і тестувати код зі швидкістю і ефективністю. Читання аргументів командного рядка - важлива навичка для кожного програміста, яка дозволяє більш ефективно працювати зі своїм кодом.

## Як виконувати

Читання аргументів командного рядка у Clojure досить просто. Для цього використовується функціонал "command-line-args", який приймає аргументи командного рядка у вигляді списку та повертає результат у вигляді вектора. Нижче наведено приклад коду та вихідної інформації:

```Clojure
(defn get-arguments []
  (command-line-args))
 
(def arguments (get-arguments))

;; Приклад запуску програми з аргументами "clojure article.md"
 
```
 
Вихідна інформація:

```Clojure
arguments
=> ["clojure" "article.md"]
```

## Глибока занурення

Для більшої гнучкості та контролю над аргументами командного рядка, можна використовувати додаткове додаткове налаштування. Наприклад, функція "parse-opts", яка дозволяє задати опції додатком на командному рядку та їх значення. Нижче наведено приклад коду та вихідної інформації:

```Clojure
(defn get-arguments []
  (parse-opts arguments
              "o" "output" "Output file name" 
              "e" "environment" "Environment variables"))

;; Приклад запуску програми з додатковими опціями "-o output.txt -e development"
 
```
 
Вихідна інформація:

```Clojure
arguments
=> {:output "output.txt" :environment "development"}
```

## Дивіться також

- [Офіційна документація Clojure](https://clojure.org/documentation)
- [Стаття про роботу з командним рядком в Clojure](https://medium.com/young-coder/working-with-command-line-arguments-in-clojure-f3d96ad37d3c)
- [Бібліотека tools.cli для парсингу аргументів командного рядка](https://github.com/clojure/tools.cli)