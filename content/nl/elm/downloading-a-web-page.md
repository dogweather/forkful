---
title:                "Een webpagina downloaden"
date:                  2024-01-28T21:59:32.971871-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een webpagina downloaden betekent gegevens van het internet rechtstreeks in je app halen om deze weer te geven of te verwerken. Programmeurs doen dit om realtime informatie te verkrijgen of dynamische inhoud aan gebruikers te bieden.

## Hoe:

Elm vereist dat neveneffecten zoals HTTP-verzoeken worden gestructureerd als commando's. Je gebruikt de `Http` module om te fetchen en de respons te verwerken.

```Elm

module Main die (main) blootlegt

importeren Browser
importeren Html die (Html, tekst) blootlegt
importeren Http

type alias Model =
    { inhoud : String }

type Msg
    = GotText (Resultaat Http.Error String)

init : ( Model, Cmd Msg )
init =
    ( Model ""
    , fetchPagina "https://api.example.com/data"
    )

fetchPagina : String -> Cmd Msg
fetchPagina url =
    Http.get { url = url, verwacht = Http.expectString GotText }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText (Ok data) ->
            ( { model | inhoud = data }, Cmd.none )

        GotText (Err _) ->
            ( { model | inhoud = "Fout: Kon pagina niet fetchen." }, Cmd.none )

weergave : Model -> Html Msg
weergave model =
    tekst model.inhoud

main : Programma () Model Msg
main =
    Browser.sandbox { init = init, update = update, weergave = weergave }

```

Bij een succesvolle fetch zal `inhoud` in je model de inhoud van de pagina bevatten. Bij een fout zal het een eenvoudige foutmelding bevatten.

## Diepgaand

Elm behandelt neveneffecten als een Data, wat betekent dat HTTP-verzoeken worden beheerd door de Elm-runtime, niet direct in je code. Historisch gezien was dit een afwijking van talen zoals JavaScript, waar neveneffecten meer vrijblijvend zijn. Alternatieven in andere talen kunnen `fetch` zijn in JavaScript of `requests` in Python. Elm's benadering zorgt ervoor dat je app voorspelbaar en onderhoudbaar blijft door neveneffecten in typen te coderen en een gecentraliseerde `update` functie te gebruiken om veranderingen te beheren.

De `Http` module bestond niet altijd in Elm. Eerdere versies bedachten hun eigen AJAX, wat omslachtig was. Nu biedt `Http` een reeks functies om verschillende gevallen te behandelen, zoals het verwachten van JSON of strings, wat het gebruiksvriendelijker maakt.

Wat betreft de implementatie, wanneer je `fetchPagina` aanroept, stuurt Elm een bericht naar je `update` functie met het resultaat. Het zal ofwel `Ok data` zijn als het slaagt of `Err fout` als het faalt. Je matched op deze uitkomsten en werkt je `Model` en weergave dienovereenkomstig bij.

## Zie Ook

- Elm's HTTP pakketdocumentatie: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Elm Gids over Effecten: [https://guide.elm-lang.org/effects/](https://guide.elm-lang.org/effects/)
- JSON Decoding in Elm (voor als de gegevens die je fetcht geen platte tekst zijn): [https://package.elm-lang.org/packages/elm/json/latest/](https://package.elm-lang.org/packages/elm/json/latest/)
