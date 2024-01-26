---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:36:09.622844-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att tolka ett datum från en sträng innebär att konvertera textformaterad data till ett datumformat som programmet kan förstå och använda. Programmerare gör detta för att enkelt hantera och manipulera datumvärden, såsom att beräkna tidsskillnader eller visa datum enligt olika tidszoner.

## How to:
Elm gör det enkelt att hantera datum genom `Date` modulen. Tänk på att Elm inte har inbyggda funktioner för datumtolkning från strängar, så vi lutar oss mot `elm/time` arkivet och kan behöva en JS interop via ports för komplexa format.

```Elm
import Time
import Json.Decode as Decode

parseDate : String -> Decoder Time.Posix
parseDate dateStr =
    Decode.map Time.millisToPosix (Decode.int)

sampleDateStr : String
sampleDateStr = "1617181920" -- Ett exempel på en UNIX-tidsstämpel i millisekunder som en sträng

-- Antag att vi får detta som en del av en större JSON-svar
type alias ApiResponse =
    { date : Time.Posix }

dateDecoder : Decode.Decoder ApiResponse
dateDecoder =
    Decode.map ApiResponse (Decode.field "date" (parseDate sampleDateStr))

-- När du kör decodern får du `Time.Posix` som du kan arbeta med i Elm.
```

Observera ovanstående kodstycke är ett enkelt exempel. För att dekoda en faktisk datumsträng från JSON, ersätt `sampleDateStr` med din JSON:s datumfält.

## Deep Dive
Elm har en striktare hantering av datum än många andra språk. Tidiga versioner av Elm hade mer omfattande datumfunktioner, men dessa förenklades för att minska komplexitet och externa beroenden. 

Alternativ till att tolka datumsträngar inkluderar att använda JavaScript-portar för att använda starkare JS-bibliotek som `Moment.js` eller `date-fns`. Elm-communityt har också skapat bibliotek som `elm-date-extra` för att hantera datum.

När Elm tolkar datum, sker det ofta i form av POSIX-tid (tid i millisekunder sedan Unix-epoken), vilket gör det jämförbart över olika system. För att tolka mer komplexa datumformat, kan anpassade decoders skapas som använder `String` manipulation följt av `String -> Int` konverteringar innan de skapar en `Time.Posix` instans.

## See Also
Här är några användbara länkar för att utforska mer:

- Elm `Time` modulen: [package.elm-lang.org/packages/elm/time/latest](https://package.elm-lang.org/packages/elm/time/latest)
