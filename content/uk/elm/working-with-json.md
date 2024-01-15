---
title:                "Робота з json"
html_title:           "Elm: Робота з json"
simple_title:         "Робота з json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Чому

Написання програм на Elm за допомогою JSON - простий і ефективний спосіб працювати з даними у веб-додатках. Використання Json в Elm дозволяє легко читати та зберігати дані в структурованому форматі, що є необхідним у сучасному програмуванні.

## Як це зробити

Для роботи з JSON у програмі Elm, спочатку потрібно встановити дані залежності за допомогою команди `elm-package install package-name`. Потім можна здійснювати JSON запити, використовуючи пакет `elm-lang/http`, а результат отримувати у вигляді Json за допомогою функції `Http.expectJson`. Нижче наведений приклад отримання та обробки Json у Elm:

```
import Json.Decode exposing (..)
import Http

type alias User =
    { id : Int
    , name : String
    }

userDecoder : Decoder User
userDecoder =
    map2 User (field "id" int) (field "name" string)

getUser : Int -> Cmd Msg
getUser userId =
    Http.get { url = "https://example.com/users/" ++ toString userId, expect = expectJson userDecoder }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserFetched result ->
            case result of
                Ok user ->
                    ( { model | user = Just user }, Cmd.none )
                Err _ ->
                    ( { model | user = Nothing }, Cmd.none )
        _ ->
            ( model, Cmd.none )
```

Отримані дані можна далі використовувати для рендерингу елементів веб-сторінки і багатьох інших задач.

## Поглиблене дослідження

JSON (JavaScript Object Notation) - це формат обміну даними, який використовується для передачі структурованих даних між різними системами. Elm надає зручний спосіб роботи з даними у цьому форматі за допомогою пакету `elm-lang/json`. Імпортувати цей пакет можна за допомогою модуля `Json`.

В Elm є два типи, які використовуються для роботи з JSON: `Decoder` і `Encoder`. `Decoder` використовується для декодування Json у структури даних Elm, тоді як `Encoder` - для кодування даних Elm у Json формат. Для цього використовуються функції `decode` і `encode`.

Крім того, Elm надає операції для роботи з різними типами даних у Json, такими як об'єкти, масиви, рядки та числа. Наприклад, для отримання значення з об'єкта використовується функція `field`, а для отримання значення з масиву - `index`.

Більш детальну інформацію про роботу з JSON у Elm можна знайти у [документації Elm](https://guide.elm-lang.org/effects/json.html).

## Дивись також

- [Офіційна документація Elm](https://elm-lang.org/docs)
- [Приклади використання JSON в Elm](https://github.com