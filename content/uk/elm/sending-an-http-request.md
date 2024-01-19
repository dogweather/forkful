---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і чому?
Відправка HTTP-запиту - це процес, коли ми, як програмісти, запитуємо інформацію з сервера або відправляємо на нього дані. Нам це потрібно, щоб отримати дані для своїх веб-додатків чи обновити існуючі дані на сервері.

## Як це зробити:
Давайте поглянемо, як в Elm відправляється HTTP-запит:

```Elm
import Http exposing (..)
import Json.Decode as Decode

getPosts : Cmd Msg
getPosts =
    Http.get
        { url = "https://jsonplaceholder.typicode.com/posts"
        , expect = Http.expectJson GotPosts (Decode.list postDecoder)
        }
```
Цей приклад показує, як відправити GET-запит до сервера і очікувати JSON відповідь.

## Поглиблений огляд:
Відправка HTTP-запиту має довгу історію і була ключовим аспектом розвитку вебу. Elm використовує концепцію команд (Cmd) для відправки HTTP-запитів - це надає нашим додаткам впевненість в отриманні відповідей, незалежно від стану мережі.

Зауважте, що Elm має альтернативи для відправки HTTP запитів, що можуть бути корисними в залежності від контексту вашого додатку. Наприклад, `Http.post`.

Щодо деталей виконання, Elm відправляє HTTP-запит у несинхронний спосіб, що дозволяє вашому додатку продовжувати роботу, незалежно від результату запиту. 

## Дивіться також:
* Документація Elm про HTTP-запити: https://package.elm-lang.org/packages/elm/http/latest/
* Введення в HTTP: https://developer.mozilla.org/uk/docs/Web/HTTP/Overview
* Довідка Elm Json.Decode: https://package.elm-lang.org/packages/elm/json/latest/Json-Decode