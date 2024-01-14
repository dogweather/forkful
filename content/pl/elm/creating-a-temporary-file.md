---
title:    "Elm: Tworzenie pliku tymczasowego"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Dlaczego warto tworzyć pliki tymczasowe w Elm?

Pisanie skryptów w Elm może być czasem nieuniknionym zadaniem, szczególnie jeśli aplikacja wymaga interakcji z systemem plików. Jedną z przydatnych umiejętności jest tworzenie plików tymczasowych, które są potrzebne do wykonania określonych operacji. W tym artykule omówimy dlaczego warto tworzyć pliki tymczasowe w Elm oraz jak to zrobić.

## Jak to zrobić?

Do tworzenia plików tymczasowych w Elm wykorzystujemy wbudowany moduł `Task` oraz zewnętrzną bibliotekę `elm/file`. Poniżej przedstawimy prosty przykład kodu, który tworzy plik tymczasowy i zapisuje w nim dane o nazwie, której użytkownik podaje przy uruchomieniu skryptu.

```elm
module Main exposing (main)

import File
import Task
import Task exposing (Task)
import File.Name as Name

type Msg
    = CreateFile
    | FileCreated (Result File.Error ())
    | SaveToTempFile String

init : () -> ( Model, Cmd Msg )
init _ =
    ( ()
    , Task.perform FileCreated CreateFile
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateFile ->
            ( model, Task.perform FileCreated (File.file Name.random []) )

        FileCreated (Ok ()) ->
            ( model, Task.succeed (SaveToTempFile "Example") )

        FileCreated (Err err) ->
            ( model
            , Task.fail
                "Nie udało się utworzyć pliku tymczasowego. Sprawdź uprawnienia do zapisu w wybranym folderze."
            )

view : Model -> Html Msg
view _ =
    text "W stworzonym pliku tymczasowym znajdują się dane o nazwie 'Example'"

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

main : Program () Model Msg
main =
    Platform.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
```

Po uruchomieniu skryptu, w wybranym folderze pojawi się plik tymczasowy o unikalnej nazwie. W przypadku błędu, użytkownik otrzyma odpowiedni komunikat.

## Głębsze spojrzenie

Tworzenie plików tymczasowych jest również przydatne w sytuacjach, gdy musimy przetwarzać dużą ilość danych i nie chcemy obciążać pamięci podręcznej przeglądarki. Wówczas możemy zapisywać dane tymczasowo w pliku, a po zakończeniu operacji usunąć go. Jest to szczególnie ważne przy tworzeniu aplikacji internetowych, gdzie każdy bajt pamięci ma znaczenie.

# Zobacz także

- Dokumentacja modułu `Task` w Elm: https://package.elm-lang.org/packages/elm/core/latest/Task
- Dokumentacja biblioteki `elm/file`: https://package.elm-lang.org/packages/elm/file/latest/