---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente den nåværende datoen betyr å få tilgang til den eksakte datoen og klokkeslettet akkurat nå i programmet ditt. Dette kan være nyttig for en rekke programmeringskrav, f.eks for å registrere tidspunktet for en bestemt hendelse i en applikasjon.

## Hvordan gjør du det:
Her er enkel kode for å hente den nåværende datoen i Elm:

```Elm
import Time
import Task
import Browser

type alias Model = 
  { time : Time.Posix }

init : flags -> ( Model, Cmd Msg )
init flags =
  ( Model Time.millisToPosix 0
  , Task.perform GetCurrentTime Time.now
  )

type Msg =
  GetCurrentTime Time.Posix
```
Koden over vil hente den nåværende datoen ved å bruke funksjonene tilgjengelig i `Time`-modulen.

## Dyp Dykk

Hente den nåværende datoen har sin rot i de tidligste dagene av databehandling. Data og tid er kritiske datatyper som brukes i nesten alle moderne applikasjon.

I Elm, vi bruker `Time` modul for å håndtere dato og tid funksjonaliteter. Den gir deg funksjonalitet til å konvertere datoen til forskjellige formater som du kan bruke for forskjellige formål i applikasjonen din.

Alternativene for å få dagens dato i Elm kan variere basert på dine spesifikke behov. For eksempel, hvis du bare er interessert i datoen, kan du konvertere `Time.Posix` verdien til en dato-streng.

Implementasjonsdetaljer i Elm er rett frem. `Time.now`-funksjonen brukes til å hente den nåværende datoen og tiden. Den returnerer en oppgave som resultat, som deretter kan utføres for å få en `Time.Posix` verdi som representerer den nåværende datoen.

## Se Også

For mer informasjon om bruk av dato og tid i Elm, kan du referere til følgende dokumenter:

1. Elm Time Modul Dokumentasjon: [https://package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
2. Introduksjon til Funksjonell Programmering i Elm: [https://github.com/dwyl/learn-elm](https://github.com/dwyl/learn-elm)
3. Elm Real World Eksempler på Github: [https://github.com/elm-community/elm-time](https://github.com/elm-community/elm-time)