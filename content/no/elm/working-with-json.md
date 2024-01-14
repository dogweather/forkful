---
title:                "Elm: Arbeide med json"
simple_title:         "Arbeide med json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med JSON kan være en nyttig og spennende opplevelse for utviklere som ønsker å utvide sine programmeringsferdigheter. JSON er en svært populær dataformat som brukes for å utveksle data mellom applikasjoner og servere. Ved å lære Elm-programmering og hvordan du arbeider med JSON, vil du kunne bygge mer dynamiske og responsive applikasjoner.

## Hvordan

For å begynne å jobbe med JSON i Elm kan du bruke den innebygde funksjonen `Json.Decode.decodeString`. Denne funksjonen tar inn en JSON-streng og konverterer den til en verdi i Elm. La oss se på et eksempel:

```Elm
fetchUserData : (Result String User -> msg) -> Cmd msg
fetchUserData msg =
    Http.get
        { url = "http://example.com/api/user/1",
          expect = Http.expectString msg
        }
```

I dette eksempelet bruker vi `Http.get` for å gjøre et HTTP-kall til en API som returnerer brukerdata for bruker nummer 1. Dette HTTP-kallet forventer en JSON-streng som respons, så vi bruker `Http.expectString` som en del av den innkommende funksjonen for å konvertere responsen til en streng i Elm-format.

For å behandle denne strengen, kan vi bruke `Json.Decode.decodeString`-funksjonen. Denne funksjonen tar inn en dekoderfunksjon, som er en funksjon som tar inn en JSON-streng og returnerer den ønskede verdien. La oss si at vi ønsker å hente ut brukerens navn fra responsen. Da kan vi lage en dekoderfunksjon som ser slik ut:

```Elm
import Json.Decode exposing (..)

type alias User =
    { name : String
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "name" string
```

Vi bruker `Json.Decode.decode` til å lage en dekoderfunksjon for typen `User`, og spesifiserer deretter hvilke felter vi ønsker å hente ut fra JSON-en (i dette tilfellet en `name`-streng). Nå kan vi bruke denne dekoderen i `Json.Decode.decodeString`-funksjonen for å konvertere responsen til en verdi i Elm:

```Elm
fetchUserData : (Result String User -> msg) -> Cmd msg
fetchUserData msg =
    Http.get
        { url = "http://example.com/api/user/1",
          expect = Http.expectString (Json.Decode.decodeString decodeUser msg)
        }
```

Nå vil `fetchUserData`-funksjonen returnere en `User`-verdi til innkommende funksjon når HTTP-kallet er fullført.

## Dypdykk

Å jobbe med JSON i Elm er ikke bare begrenset til enkeltstående kall til serveren. Du kan også håndtere mer komplekse JSON-strukturer ved hjelp av ulike dekoderfunksjoner og Elm-strukturer som ligner på den vi så på i eksempelet vårt. Med Elm kan du enkelt strukturere og behandle vanlige JSON-datatyper som strenger, tall, boolske verdier og lister.

Du kan også jobbe med innkapslede datatyper i JSON, som kan være spesielt nyttig når du arbeider med API-er. Elm gjør det enkelt å dekode JSON-strukturer som `Maybe`, `List` og `Result`, som gjør det enklere å håndtere feil og ikke-eksisterende data.

## Se også

* [Elm dokumentasjon om JSON dekoding](https://elm-lang.org/docs/json)
* [JSON i Elm - eksempelprosjekt](https://github.com/elm/json)
* [Her er en god guide til å lære Elm-programmering](https://guide.elm-lang.org/)