---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego to proces, w którym program wyciąga dane z zewnętrznego źródła pliku tekstowego. Programiści robią to, kiedy potrzebują dostać się do konkretnych informacji, które mogą być przechowywane w pliku.

## Jak to zrobić:

```Elm
import Http
import Json.Decode

type alias Model =
    { content : String
    }

init : ( Model, Cmd Msg )
init =
    ( Model "", Http.send GotFile (Http.get "file.txt" Json.Decode.string) )

type Msg
    = GotFile (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFile (Ok s) ->
            ({ model | content = s }, Cmd.none)

        GotFile (Err e) ->
            ({ model | content = toString e }, Cmd.none)
```

W powyższym przykładzie czytamy plik `file.txt` i przypisujemy jego zawartość do struktury `Model`.

## Pogłębiona analiza

Czytanie pliku tekstowego jest jednym z najbardziej podstawowych zadań, które programista może wykonać. Elm, będący funkcjonalnym językiem programowania skierowanym na przeglądarkę, różni się od większości innych języków, ponieważ nie oferuje natywnej możliwości czytania plików. Zamiast tego, korzysta z requestów HTTP do uzyskania danych.

Podczas gdy tradycyjne języki jak Python lub Java mogą czytać pliki bezpośrednio z dysku twardego, Elm musi korzystać z serwera. Jest to związane z bezpieczeństwem przeglądarek internetowych - nie chcemy, aby dowolna strona mogła czytać dowolny plik z dysku twardego użytkownika.

Dlatego w Elm, aby przeczytać plik, musimy go najpierw udostępnić przez serwer, a następnie zrobić request HTTP, jak pokazano powyżej.

## Zobacz też

1. Dokumentacja Elm Http: https://package.elm-lang.org/packages/elm/http/latest/
2. Jak używać Json.Decode w Elm: https://guide.elm-lang.org/effects/json.html
3. Tokarzewski, S. (2016). Elm - język nowej generacji. Helion. Link: https://helion.pl/ksiazki/elm-jezyk-nowej-generacji-sebastian-tokarzewski,elmnja.htm
4. Capan, I. (2017). Praktyczne programowanie w Elm. PWN. Link: https://pwn.pl/produkt/praktyczne-programowanie-w-elm,180278030.html