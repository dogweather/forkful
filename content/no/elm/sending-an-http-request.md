---
title:                "Elm: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sende HTTP-forespørsler kan være en viktig del av å utvikle en moderne webapplikasjon. Dette kan være nyttig for å hente data fra en ekstern server eller utføre handlinger som påvirker brukerens erfaring på nettsiden.

## Slik gjør du det

For å sende en HTTP-forespørsel i Elm, må du bruke "Http" biblioteket. Du må også importere Http-pakkene som inneholder funksjoner for å sende og behandle forespørsler.

```
import Http
import Http exposing (..)
```

Deretter kan du bruke funksjonen "send" for å sende en forespørsel. Du må også spesifisere metoden (GET, POST, osv.), URLen og eventuelle data som skal sendes med forespørselen.

```
send : String -> String -> Http.Request a (Maybe b) -> Cmd b
```

Et eksempel på å utføre en GET-forespørsel til en API som returnerer en liste over brukere ser slik ut:

```
Http.get "https://api.example.com/users"
    |> Http.send getUsers
```

Her vil getUsers fungere som et signal som mottar responsen fra APIen. Deretter kan du håndtere responsen ved å bruke en funksjon som "Http.expectJson" for å dekode responsen og få ut ønsket data.

```
Http.expectJson getUsers responseDecoder
```

## Dypdykk

For å sende en POST-forespørsel, må du spesifisere HTTP-metoden og sette dataene du vil sende i en Http.Request. Eksempelet nedenfor viser hvordan du kan sende data til en server og behandle responsen.

```
let
    user =
        { name = "John", age = 30 }

    userToRequestData user =
        Http.jsonBody user
            |> Http.send users

in
userToRequestData user
    |> Http.expectJson users responseDecoder
```

Det er også muligheter for å sette tilpassede HTTP-hoder og sende filer med en forespørsel ved bruk av forskjellige funksjoner i "Http" biblioteket.

## Se også

- Offisiell dokumentasjon for "Http" biblioteket: https://package.elm-lang.org/packages/elm/http/latest/
- Elm Guide om å jobbe med APIer: https://guide.elm-lang.org/effects/http.html