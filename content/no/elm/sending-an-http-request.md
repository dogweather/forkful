---
title:                "Å sende en http-forespørsel"
html_title:           "Elm: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Når en programmerer sender en HTTP forespørsel, betyr det at de ber om informasjon fra en annen datamaskin eller server. Dette kan være nyttig for å hente data til en nettside eller en app. Programmere gjør dette for å få tilgang til informasjon utenfor deres egen kode og for å lage dynamiske og responsive applikasjoner.

## Hvordan:

```Elm
sendHTTPrequest : String -> Cmd Msg
sendHTTPrequest url =
    Http.get
        { url = url
        , expect = Http.expectString handleResponse
        }

handleResponse : Http.Response String -> Msg
handleResponse response =
    case response of
        Ok body ->
            -- Hvis forespørselen var vellykket, kan vi gjøre noe med svaret her
            SuccessMsg body

        Err _ ->
            -- Håndter feil hvis forespørselen mislyktes
            FailureMsg

-- For å faktisk sende forespørselen, må vi kalle på funksjonen vår og gi den en url å sende til:
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }

-- Her sender vi en forespørsel og lagrer svaret i vår modell:
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendRequest ->
            ( model, sendHTTPrequest "https://eksempel.com/getdata" )
        SuccessMsg body ->
            ( { model | data = body }, Cmd.none )
        FailureMsg ->
            ( model, Cmd.none )

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendRequest ] [ text "Send forespørsel" ]
        , p [] [ text model.data ]
        ]
```

## Dykk Dypere:

Å sende HTTP forespørsler er en viktig del av moderne webutvikling, da det gjør det mulig for programmerere å lage dynamiske applikasjoner som kan kommunisere med andre servere for å hente informasjon. Alternativer til å bruke Elm sin innebygde http-modul inkluderer å bruke en backend server eller andre JavaScript-baserte biblioteker som Axios.

For å implementere en HTTP forespørsel i Elm, bruker vi funksjonene som er tilgjengelige i Elm sin `Http` pakke. Dette inkluderer en funksjon for å sende GET, POST, PUT og DELETE forespørsler, samt en funksjon for å forvente en bestemt type svar fra serveren. Det er også mulig å håndtere eventuelle feil ved å bruke `Http.expectString` funksjonen.

## Se også:

[Offisiell Elm-Dokumentasjon om HTTP](https://package.elm-lang.org/packages/elm/http/latest/)

[Elm Guide for nettverksforespørsler](https://guide.elm-lang.org/effects/http.html)

[Tutorial om å sende HTTP forespørsler i Elm](https://dev.to/vicrazumov/sending-requests-in-elm-part-2-37kn)