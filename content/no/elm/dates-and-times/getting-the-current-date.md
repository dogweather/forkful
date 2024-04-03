---
date: 2024-01-20 15:14:18.979784-07:00
description: "\xC5 hente n\xE5v\xE6rende dato betyr \xE5 tilgjengeliggj\xF8re akkurat\
  \ hvilken dag det er, helt ned til millisekundet, i Elm-programmet ditt. Dette er\
  \ nyttig for alt\u2026"
lastmod: '2024-03-13T22:44:40.719933-06:00'
model: unknown
summary: "\xC5 hente n\xE5v\xE6rende dato betyr \xE5 tilgjengeliggj\xF8re akkurat\
  \ hvilken dag det er, helt ned til millisekundet, i Elm-programmet ditt."
title: "Slik f\xE5r du tak i dagens dato"
weight: 29
---

## How to:
Elm har ikke innebygd dato-behandling, så du må bruke `elm/time` biblioteket. Her er et eksempel på hvordan du kan få den nåværende datoen:

```Elm
import Time
import Task
import Browser

type Msg = Tick Time.Posix

main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

init : () -> ( (), Cmd Msg )
init _ =
    ( (), Task.perform Tick Time.now )

update : Msg -> model -> ( model, Cmd Msg )
update (Tick currentTime) model =
    ( model, Cmd.none )

view : model -> Html.Html Msg
view model =
    -- Gjerne legg til kode her for hvordan du vil vise datoen på skjermen.
    Html.text "Her ville den nåværende datoen dukke opp"

subscriptions : model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick -- Oppdaterer hver sekund.
```

Dette koden vil kjøre en `Tick` melding hver sekund med den nåværende datoen som `Time.Posix` verdi.

## Deep Dive
Elm er et funksjonelt språk som standardiserer håndteringen av tid gjennom `elm/time` biblioteket. Hvis du kommer fra JavaScript-verdenen, er det verdt å merke seg at Elm behandler sideeffekter, som å hente nåværende dato, annerledes. Innføring av `elm/time` var en måte å gi Elm ren tidshåndtering samtidig som språket holder seg funksjonelt og forutsigbart.

Tidligere tilnærminger, som `Date` i JS, har mange bivirkninger og inkonsistenser. Elm forenkler dette ved å introdusere `Time.Posix`, som representerer tidspunkter i den såkalte POSIX formatet (antall millisekunder siden midnatt, 1. januar 1970 UTC).

Det finnes også alternativer til `elm/time` for mer kompleks dato-håndtering, som `justinmimbs/date` for Elm, som utvider funksjonaliteten for å inkludere operasjoner på kalenderdatoer.

## See Also
- Elm `Time` modul dokumentasjon: https://package.elm-lang.org/packages/elm/time/latest/
- Elm diskusjonsforum for spesifikke spørsmål: https://discourse.elm-lang.org/
- 'justinmimbs/date' bibliotek for flere dato-operasjoner: https://package.elm-lang.org/packages/justinmimbs/date/latest/
