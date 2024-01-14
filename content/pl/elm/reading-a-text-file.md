---
title:    "Elm: Odczytywanie pliku tekstowego"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz nauczyć się programowania w języku Elm lub po prostu poszerzyć swoją wiedzę na ten temat, przeczytaj ten wpis! Dowiecie się, jak wczytywać pliki tekstowe i wykorzystać tę umiejętność w swoich programach. To ważna umiejętność dla każdego programisty!

## Jak to zrobić

Aby wczytać plik tekstowy w Elm, musisz użyć funkcji `Http.get` i `File.toText`. Pokażę Ci to na przykładzie. Załóżmy, że chcesz wczytać plik `hello.txt` z folderu `pliki_tekstowe`.

```Elm
import Http
import File

type Msg
    = GotText (Result Http.Error String)

getFile : Cmd Msg
getFile =
    File.toText "pliki_tekstowe/hello.txt"
        |> Task.perform GotText
```

Następnie, aby wyświetlić zawartość pliku, użyjemy funkcji `case` w funkcji `update`:

```Elm
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok text ->
                    ( { model | content = Just text }, Cmd.none )

                Err error ->
                    ( { model | content = Just "Błąd podczas wczytywania pliku" }, Cmd.none )

        _ ->
            ( model, Cmd.none )
```

Teraz, gdy uruchomisz ten kod, powinien bez problemu wczytać zawartość pliku tekstowego i wyświetlić ją w modelu Twojej aplikacji.

## Głębsze zanurzenie

Wczytywanie plików tekstowych może być trochę bardziej skomplikowane, gdy funkcja `Http.get` wysyła żądanie do adresu URL zamiast lokalnego pliku. W takim przypadku musimy zastosować pewne triki i dostosować kod do naszych potrzeb. Więcej o tym znajdziesz w [oficjalnej dokumentacji Elm](https://package.elm-lang.org/packages/elm/file/latest/).

## Zobacz także

- [Oficjalna dokumentacja Elm](https://package.elm-lang.org/packages/elm/file/latest/)
- [Wczytywanie plików CSV w Elm](https://dev.to/refactorius/wczytywanie-plikow-csv-w-elm-3fe7) (artykuł w języku angielskim)
- [Projekt Elm File Explorer](https://github.com/FreedomMan/univision-file-explo) (projekt na GitHubie)