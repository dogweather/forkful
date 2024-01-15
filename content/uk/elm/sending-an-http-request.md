---
title:                "Надсилання http-запиту"
html_title:           "Elm: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP запиту є необхідною частиною багатьох веб-додатків, оскільки це дозволяє отримати або надіслати дані з серверу. Elm надає простий та безпечний спосіб для цього, дозволяючи розробникам ефективно збирати, обробляти та відправляти дані.

## Як

```Elm
import Http
import Json.Decode exposing (Decoder)

type alias User = {
    id: Int,
    name: String
}

type Msg = 
    UsersFetched (Result Http.Error (List User))

fetchUsers : Cmd Msg
fetchUsers = 
    Http.get {
        url = "http://example.com/users",
        expect = Http.expectJson UsersFetched userDecoder
    }

userDecoder : Decoder (List User)
userDecoder = 
    Json.Decode.list (Json.Decode.map2 User
        (Json.Decode.field "id" Json.Decode.int)
        (Json.Decode.field "name" Json.Decode.string)
    )
```

Цей код показує, як використовувати `Http` модуль для надсилання GET запиту до серверу та отримання списку користувачів. `expectJson` дозволяє вказати очікуваний формат даних та декодер, який перетворить отриману відповідь в коректний тип даних. У даному випадку, ми очікуємо, що сервер поверне список об'єктів типу `User`, який буде декодований за допомогою `userDecoder`.

## Deep Dive

Для відправки HTTP запитів в Elm, використовується `Http.send` функція, яка приймає команду та повертає `Cmd` значення. Це означає, що надсилання запиту відбувається асинхронно, а результат буде отриманий через обробник повідомлень `update` функції.

Крім того, Elm надає можливість налаштувати запит за допомогою другого параметра `Http` функції, де можна вказати додаткові налаштування, такі як тип запиту, заголовки або тіло запиту. Повний список налаштувань можна знайти у [документації Elm](https://package.elm-lang.org/packages/elm/http/latest/Http).

## Дивіться також

- [Документація Elm щодо надсилання HTTP запитів](https://package.elm-lang.org/packages/elm/http/latest/Http)
- [Стаття "Надсилання HTTP запитів у Elm" на Medium](https://medium.com/@thejameskyle/sending-http-requests-in-elm-4cb00a290c66)