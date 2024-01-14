---
title:                "Elm: Відправка http-запиту"
simple_title:         "Відправка http-запиту"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

У цій статті я хочу розказати про те, як виконувати HTTP-запити в Elm. Це дуже корисно для з'єднання з сервером і отримання даних, а також для взаємодії зі зовнішніми сервісами.

## Як

Для початку, нам потрібно встановити пакет `elm/http` з Elm пакетного менеджера. Потім ми можемо використовувати функцію `Http.send` для створення нашого HTTP-запиту. Наприклад, якщо ми хочемо отримати JSON дані зі стороннього ресурсу, ми можемо написати наступний код:

```Elm
import Http
import Json.Decode exposing (decodeValue, int, string)

sendRequest : Cmd Msg
sendRequest =
    Http.send GotData (Http.get "https://example.com/data.json" decoder)

decoder : Json.Decode.Decoder Int
decoder =
    decodeValue int
```

Цей код використовує функцію `get` для створення GET-запиту на ресурс по вказаному URL і декодує дані у визначений нами формат - це може бути будь-який тип данних. Якщо запит буде успішно виконаний, результат буде переданий в нашу функцію `GotData` разом з даними, які ми можемо обробити.

## Глибока занурення

Існує багато різних варіантів виконання HTTP-запитів в Elm. Наприклад, ми можемо використовувати функцію `post` для виконання POST-запиту, або `request` для створення більш специфічного запиту з додатковими параметрами. Також можна використати функції для обробки помилок, відправки куків, аутентифікації і багато іншого.

Але важливо пам'ятати кілька моментів, коли ми працюємо з HTTP-запитами в Elm. Воно здійснюється асинхронно, тому ми не можемо відправляти декілька запитів одночасно. Також HTTP-запити виконуються від імені браузера, тому з них не можна отримати доступ до розширеного функціоналу, такого як читання файлів або доступ до бази даних.

## Дивись також

- [Документація Elm пакета `elm/http`](https://package.elm-lang.org/packages/elm/http/latest/)
- [Стаття "Sending HTTP requests in Elm" на learnxinyminutes.com](https://learnxinyminutes.com/docs/elm/)
- [Стаття "HTTP Requests with Elm" на dev.to](https://dev.to/joelthomas/sending-http-requests-in-elm-1i99)