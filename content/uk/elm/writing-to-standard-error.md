---
title:                "Запис в стандартний потік помилок"
html_title:           "Arduino: Запис в стандартний потік помилок"
simple_title:         "Запис в стандартний потік помилок"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Що це таке & Чому?
Стандартна помилка (stderr) - для повідомлень про помилки та діагностику. Використовується, щоб розділити нормальний вивід програми та повідомлення про помилки.

## Як це зробити:
Elm не має прямого доступу до стандартної помилки через чистоту функцій. Замість цього використовуємо JavaScript через порти:

```Elm
port module Main exposing (..)

-- Визначаємо порт для відправки повідомлень у stderr через JavaScript
port error : String -> Cmd msg

-- Відправляємо повідомлення через порт
sendError : String -> Cmd msg
sendError message =
    error message

```

```JavaScript
// Підписуємося на порт Elm у JavaScript
app.ports.error.subscribe(function(message) {
    console.error(message);
});
```

## Поглиблено:
Історично stderr використовувалася для зрушення збереження журналів помилок від регулярного виводу. В Elm, через парадигму чистоти, прямий доступ до системних можливостей обмежений. Порти - місток між Elm та JavaScript для взаємодії. Варіанти реалізації включають: WebSockets, HTTP запити, і LocalStorage.

## Дивись також:
- [Elm порти](https://guide.elm-lang.org/interop/ports.html)
- [Console.error() в MDN](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)