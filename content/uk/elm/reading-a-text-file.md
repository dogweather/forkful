---
title:    "Elm: Читання текстового файлу."
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Чому

Програмування на Elm може бути захоплюючим і корисним хобі для багатьох людей. Читання текстових файлів може бути корисною навичкою, що допоможе вам створювати більш динамічні та потужні програми. Якщо ви зацікавлені у вивченні цієї навички, то цей блог-пост стане вам у пригоді.

## Як це зробити

Для читання текстових файлів у Elm вам знадобиться використати стандартну бібліотеку інтернет-запитів `Http`. Для початку створимо новий файл з розширенням `.elm`. Перш ніж почати кодувати, переконайтеся, що ви встановили Elm та відкрили його у вашому улюбленому терміналі. У цьому файлі ми імпортуємо необхідні модулі та створюємо корельований тип даних.

```Elm
import Http
import Json.Decode exposing (field, map, string)

type alias TextFile = 
    { content : String }

```

Потім ми можемо написати функцію, яка буде здійснювати запит до текстового файлу за допомогою методу `get`. У нашому випадку, ми передаємо `url` текстового файлу та перетворюємо результат у наш тип даних `TextFile`.

```Elm
fetchTextFile : String -> Task Http.Error TextFile
fetchTextFile url =
    Http.get url textDecoder
        |> Task.map .content

textDecoder : Decoder TextFile
textDecoder =
    map TextFile (field "content" string)
```

Тепер, коли ми маємо функцію, ми можемо використати її в нашому `update` та оновлювати стан за допомогою результатів запиту.

```Elm
type Msg
    = FetchTextFile
    | SetTextFile (Result Http.Error TextFile)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchTextFile ->
            ( model, fetchTextFile "http://example.com/file.txt" |> Task.attempt SetTextFile )

        SetTextFile (Ok result) ->
            ( { model | textFile = result }, Cmd.none )

        SetTextFile (Err error) ->
            ( model, Cmd.none )
```

Ви можете запустити цей код на пробіжці і подивитися результат у вашому браузері розкривши консоль розробника. Вона має містити текстовий файл який ви зчитали.

## Глибоке дослідження 

Для більш глибшого розуміння зчитування текстових файлів у Elm, варто заглибитися у більш складні ситуації, наприклад коли потрібно читати файли з різними розширеннями, обробляти помилки або читати файл з мережі. Навчання цих навичок може бути корисним для вас та покращить вашу вміння створювати динамічні програми на Elm.

## Подивіться також

- [Офіці