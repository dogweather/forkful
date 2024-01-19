---
title:                "Створення тимчасового файлу"
html_title:           "C: Створення тимчасового файлу"
simple_title:         "Створення тимчасового файлу"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Що і навіщо?
Створення тимчасового файлу - це процес запису даних, які потрібні під час виконання програми, в тимчасовий файл для подальшого використання. Програмісти роблять це, щоб зменшити використання пам'яті й оптимізувати продуктивність.

## Як це зробити:
```Elm
module Main exposing (..)

import File exposing (..)
import Task exposing (Task, onError, perform, succeed)

temporaryFile : String -> String -> Task x File
temporaryFile fileName contents =
    Task.succeed (File "tmp/" ++ fileName contents)

writeTemporaryFile : String -> String -> Cmd msg
writeTemporaryFile fileName contents =
    temporaryFile fileName contents
        |> perform (always (FileWrite "tmp/" ++ fileName contents))

-- приклад використання
main : Cmd msg
main =
    writeTemporaryFile "temp" "Деякий тимчасовий текст"
```
Цей код створює функцію `writeTemporaryFile`, яка створює тимчасовий файл з деяким текстом. Однак слід зазначити, що Elm не має вбудованих можливостей для роботи з файловою системою. Замість цього ви маєте використовувати порти.

## Поглиблений аналіз
Створення тимчасових файлів було важливою частиною програмування ще з часів, коли обмеження апаратного забезпечення не дозволяли тримати великі обсяги даних в пам'яті. Цей принцип і зараз залишається актуальним через необхідність оптимізації.

Однією з альтернатив є використання баз даних для збереження тимчасових даних. Проте й тимчасові файли в деяких випадках є зручнішими і менш вимогливими до ресурсів.

У Elm немає підтримки файлової системи, тому при роботі з файлами ми змушені використовувати порти й підключати JavaScript для допомоги.

## Дивіться також
[Робота з файлами в Elm](https://guide.elm-lang.org/effects/files.html)
[Детальніше про порти в Elm](https://guide.elm-lang.org/interop/ports.html)