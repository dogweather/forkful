---
title:    "Elm: Подання текстового файлу"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Чому

Початкова мотивація комуствати текстовий файл в Elm полягає в тому, щоб створити структуроване і сумісне зі стандартами API для зберігання та обробки даних.

## Як

```Elm
module Main exposing (main)

import File
import Html exposing (Html, text)
import Task exposing (Task, andThen)

main : Program () Model Msg
main =
    Html.program
        { init = ((), loadDataFromFile)
        , view = view
        , update = update
        , subscription = Sub.none
        }

type alias Model =
    { text : String
    }


type Msg
    = FileLoaded (Result File.Error String)

loadDataFromFile : Model -> (Model, Cmd Msg)
loadDataFromFile model =
    (model, File.read "myfile.txt" FileLoaded)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FileLoaded result ->
            case result of
                Ok data ->
                    ( { model | text = data }, Cmd.none )

                Err error ->
                    ( { model | text = "Error loading file." }, Cmd.none )

view : Model -> Html Msg
view model =
    text model.text
```

В цьому прикладі ми імпортуємо модуль File, щоб отримати доступ до функції `read` для завантаження даних з файлу. Потім ми створюємо програму, яка викликає цю функцію в функції `loadDataFromFile` при запуску. Після того, як дані завантажено, ми оновлюємо модель і відображаємо текстовий файл за допомогою функції `text`.

## Глибокий пір

Підходи до створення текстових файлів можуть варіюватися в залежності від вимог проекту. Деякі корисні підходи включають використання більш потужних модулів, наприклад `elm/parser` для читання та парсингу структурованого тексту, або `elm/html` для генерації HTML сторінок.

## Дивитися також

- [Туторіал по Elm](https://guide.elm-lang.org/)
- [Документація по модулю File](https://package.elm-lang.org/packages/elm/file/latest/File)
- [Модуль для парсингу тексту](https://package.elm-lang.org/packages/elm/parser/latest/)