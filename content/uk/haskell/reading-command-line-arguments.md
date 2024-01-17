---
title:                "Читання аргументів командного рядка"
html_title:           "Haskell: Читання аргументів командного рядка"
simple_title:         "Читання аргументів командного рядка"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що & Чому?

Зчитування аргументів командного рядка - це процес отримання інформації, яку користувач передає програмі під час її запуску через командний рядок. Це корисна техніка для програмістів, яка дозволяє їм взаємодіяти з програмою та передавати їй змінні значення для виконання певних завдань.

## Як?

```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("Передані аргументи командного рядка: " ++ show args)
```

Приклад вхідних даних та виведення:

```Haskell 
$ ghc command_line_arguments.hs
$ ./command_line_arguments 1 2 3
Передані аргументи командного рядка: ["1","2","3"]
```

## Глибоке Занурення

Зчитування аргументів командного рядка стало популярним з появою платформи Unix у 1970-х роках. Такий підхід дозволяє використовувати командний рядок для зміни поведінки програми без необхідності змінювати сам код. Існують альтернативні способи передавання змінних значень, такі як оточення та конфігураційні файли, але командний рядок залишається одним з найпростіших та найшвидших способів.

## Дивись Також

- [Офіційна документація Haskell](https://www.haskell.org/documentation/)
- [Огляд командного рядка в Haskell](https://wiki.haskell.org/Command_line_option_syntax)
- [Порівняння зчитування командного рядка в різних мовах програмування](https://medium.com/geekculture/different-ways-of-parsing-command-line-arguments-in-common-programming-languages-go-java-clojure-4388ae6c5d67)