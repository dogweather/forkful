---
title:                "Att få dagens datum"
html_title:           "Elm: Att få dagens datum"
simple_title:         "Att få dagens datum"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumet kan vara användbart i många situationer, som att visa datumet på en hemsida eller att göra beräkningar baserat på dagens datum. Det är också ett bra sätt att öva på programmering i Elm och lära sig om tids- och datumfunktioner.

## Så här gör du

För att få den nuvarande datumet i Elm, behöver vi använda modulen `Time`. Först måste vi importera denna modul i början av vår kod. Sedan kan vi använda funktionen `now` för att få den nuvarande tidpunkten. Här är ett enkelt exempel:

```Elm
import Time exposing (..)

view : Html msg
view =
    div []
        [ h1 [] [ text "Dagens datum:" ]
        , text (toString (now |> toLocal |> toUtc |> toDate)) -- konverterar till lokal tidzon och sedan till datum
        ]
```

Output: 
Dagens datum: 2021-09-18

Vi kan också anpassa formatet på datumet genom att använda funktionen `format` och ge den ett önskat format som en strängparameter. Här är ett exempel där vi vill ha formatet "ÅÅÅÅ.MM.DD":

```Elm
text (now |> toTimezone +120 |> format "%Y.%m.%d") -- konverterar till en specifik tidzon och sedan formaterar datumet
```

Output:
2021.09.19

## Djupdykning

För att förstå hur funktionerna `toLocal`, `toUtc`, `toTimezone` och `format` fungerar kan vi titta närmare på typdefinitionerna för tidsstämplarna som returneras av `now`-funktionen:

```Elm
type Time.Posix = Milliseconds

type alias Time.Zone = { hours : Int, minutes : Int }

type alias Time.Date = { year : Int, month : Int, day : Int }

type Time.Timestamp = Time.Zone -> Time.Date
```

`Time.Posix` representerar antalet millisekunder sedan 1 januari 1970. `Time.Date` innehåller information om år, månad och dag. `Time.Zone` representerar en tidzon i antal timmar och minuter från UTC. Och `Time.Timestamp` är en funktion som tar en tidzon som indata och returnerar ett `Time.Date`-värde.

Genom att kombinera dessa funktioner kan vi manipulera och formatera datumet enligt våra behov.

## Se även

- Elm dokumentation för [Time-modulen](https://package.elm-lang.org/packages/elm/time/latest/)
- Elm för Nybörjare: [Tids- och Datumbehandling](https://guide.elm-lang.org/effects/time.html)
- [Datum och Tid i Elm](http://simonh1000.github.io/2016/05/elm-date-and-time-in-elm.html) av Simon Hampton