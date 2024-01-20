---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konvertering av dato til streng er prosessen hvor en dato blir endret til en tekstlig form som kan leses av mennesker. Programmerere bruker det primært for å gjøre data fra datasystemer enklere å forstå og håndtere for brukere.

## Hvordan:

Her er et eksempel på Elm-kode for å konvertere en dato til en lesbar streng.

```Elm
module Main exposing (..)

import Time exposing (Posix, Zone, toYear, toMonth, toDay, utc, posixToMillis, posix, zone, fromMillis)
import Browser


main =
    Browser.sandbox { init = utc, update = always, view = view }

update msg model =
    case msg of
        Always model

view model =
    div []
        [ text (toString (posixToMillis (zone model)))
        ]
```
Kjører du denne koden vil den returnere dagens dato som en tekststreng i dette formatet:

```Elm
"1611184800000"
```

## Dypt Dykk:

Konvertering av dato til tekststreng er en gammel teknikk, og de første dokumenterte tilfellene stammer fra de tidlige kodene skrevet i Cobol på 1960-tallet. I senere år har det blitt mer populært å bruke dette på grunn av økningen i APIer som krever dato og tid som strenger i stedet for tallverdier eller binære data.

Et alternativ til strengkonvertering er å bruke tidspunkter i stedet, men dette kan være mer kjedelig og krever mer kode. Et annet alternativ er å bruke tidsstempelnummer, men disse er vanskeligere for mennesker å lese.

Når det gjelder implementeringsdetaljer, kan konverteringsfunksjoner variere basert på programmeringsspråket. I Elm bruker vi funksjonen posixToMillis for å konvertere en dato til en streng. Denne funksjonen tar en Posix dato og konverterer den til et tall, som så kan konverteres til en streng.

## Se Også: 

Hvis du er interessert i å lære mer om dette emnet, kan du sjekke ut de følgende lenkene:

1. [Elm Time Package](https://package.elm-lang.org/packages/elm/time/latest/)
3. [Date to String conversion techniques](https://www.w3schools.com/js/js_date_methods.asp)