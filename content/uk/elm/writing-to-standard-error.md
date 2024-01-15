---
title:                "Написання до стандартного потоку помилок"
html_title:           "Elm: Написання до стандартного потоку помилок"
simple_title:         "Написання до стандартного потоку помилок"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Чому 

Запис до стандартного помилкового потоку (standard error) може бути корисним, коли ви хочете дізнатись про помилки, які виникли під час виконання програми. Він також може бути використаний для відладки інших складних проблем, що можуть виникнути під час розвитку програми.

## Як це зробити 

```Elm
import Debug

main =
  let
    result = divide 10 0
  in
    Debug.crash ("Помилка ділення: " ++ (Debug.toString result))
```

Результат: `Помилка ділення: Infinity`

Іншим способом запису до стандартного помилкового потоку є використання команди `Debug.log`, яка приймає два параметри: повідомлення та значення, яке потрібно вивести.

```Elm
import Debug

main =
  let
    result = divide 10 0
  in
    Debug.log "Помилка ділення" result
```

Результат: `Помилка ділення: Infinity`

## Глибока підкладка 

У Elm є кілька способів запису до стандартного помилкового потоку. Основний - використання функцій з модуля `Debug`, але також можна отримати доступ до потоку напряму, використовуючи модуль `Platform`. Для цього потрібно додати модуль `Native.Platform` і викликати функцію `Native.Platform.crash`, яка приймає повідомлення як параметр.

```Elm
import Native.Platform

main =
  Native.Platform.crash "Помилка!"

```

Цей спосіб може бути корисним, якщо ви працюєте з іншими модулями, які не мають доступу до функцій з модуля `Debug`.

## Дивіться також 

- [Офіційна документація Elm: Debug Module](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Офіційна документація Elm: Platform Module](https://package.elm-lang.org/packages/elm/core/latest/Platform)