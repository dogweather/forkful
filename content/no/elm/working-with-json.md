---
title:                "Jobbe med json"
html_title:           "Elm: Jobbe med json"
simple_title:         "Jobbe med json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Å jobbe med JSON kan åpne opp mange muligheter i utviklingen av interaktive webapplikasjoner. JSON er et populært format for å overføre data mellom klient og server, og ved å lære hvordan man håndterer JSON i Elm, kan man få enda mer fleksibilitet og funksjonalitet i sine prosjekter.

## Hvordan

Å håndtere JSON i Elm er en relativt enkel prosess. La oss ta en titt på et eksempel der vi henter data fra et API og presenterer det i en liste på nettsiden.

Først trenger vi å importere `Json.Decode` modulen i vår Elm-fil. Dette gjøres ved å legge til følgende linje på toppen av filen:

```Elm
import Json.Decode
```

Deretter kan vi opprette en funksjon som henter data fra et API ved hjelp av `Http.get` funksjonen. Vi kan også definere en decoder funksjon som vil konvertere JSON dataen til Elm datastrukturer. Følgende eksempel viser hvordan dette kan gjøres:

```Elm
getData : Cmd Msg
getData =
   Http.get
      { url = "https://api.example.com/users"
      , expect = Http.expectJson UserListDecoder
      }

type alias User =
   { name : String
   , age : Int
   }

type alias UserList =
   List User

userDecoder : Json.Decode.Decoder User
userDecoder =
   Json.Decode.succeed User
      |> Json.Decode.required "name" Json.Decode.string
      |> Json.Decode.required "age" Json.Decode.int

userListDecoder : Json.Decode.Decoder UserList
userListDecoder =
   Json.Decode.list userDecoder

type Msg
   = UserListLoaded (Result Http.Error UserList)

update msg model =
   case msg of
      UserListLoaded result ->
         case result of
            Ok userList ->
               -- do something with the list of users

            Err error ->
               -- handle error case
```

Til slutt trenger vi å kalle på `getData` funksjonen i vår `update` funksjon for å hente dataen. Når vi får en melding tilbake fra `getData`, vil `UserListLoaded` bli utløst, og vi kan fange opp resultatet og gjøre noe med det.

## Dypdykk

Når man jobber med JSON i Elm, kan det være nyttig å vite om noen av funksjonene i `Json.Decode` modulen som ikke ble brukt i det enkle eksempelet over. For eksempel kan man bruke `oneOf` funksjonen for å definere flere alternative decoder funksjoner, avhengig av hvilken struktur det aktuelle JSON dataen har.

I tillegg til å bruke `Http.get` for å hente data fra et API, kan man også bruke `Http.post`, `Http.put`, `Http.delete` eller andre funksjoner avhengig av hva som er nødvendig i et prosjekt.

Det er også verdt å nevne at Elm's type system vil hjelpe deg å sikre at alle feltene i ditt `User` objekt er til stede i JSON dataen, noe som kan forhindre feil under utviklingsprosessen.

## Se også

For flere ressurser om å jobbe med JSON i Elm, kan du sjekke ut følgende linker:

- [Offisiell Elm dokumentasjon om JSON](https://package.elm-lang.org/packages/elm/json/latest/)
- [Tutorial om å håndtere JSON i Elm](https://guide.elm-lang.org/effects/json.html)
- [Eksempelprosjekt for håndtering av JSON i Elm](https://github.com/elm-community/json-extra)