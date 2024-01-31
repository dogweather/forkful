---
title:                "Avrunding av tall"
date:                  2024-01-26T03:45:27.594801-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrunding av tall"

category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/rounding-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å avrunde tall er å justere et desimaltall til nærmeste hele verdi eller til et spesifisert antall desimaler. Programmerere avrunder for å redusere kompleksitet, forbedre lesbarhet, eller møte presisjonskrav.

## Hvordan:

Elms `Basics`-modul tilbyr praktiske funksjoner for avrunding: `round`, `floor` og `ceiling`. Slik bruker du dem.

```elm
import Basics exposing (round, floor, ceiling)

-- Avrund til nærmeste hele tall
round 3.14    --> 3
round 3.5     --> 4

-- Avrund nedover
floor 3.999   --> 3

-- Avrund oppover
ceiling 3.001 --> 4

-- Trunker desimaler uten avrunding
truncate 3.76 --> 3
```

Elm tilbyr også `toLocaleString` for avrunding til et fast antall desimaler:

```elm
import Float exposing (toLocaleString)

-- Avrund til to desimaler
toLocaleString 2 3.14159 --> "3.14"
```

## Dypdykk

Elm er et sterkt typet funksjonelt språk som legger sideeffekter til arkitekturens "kanter". Dette betyr at funksjoner som avrunding må være rene og forutsigbare. Historisk sett er avrunding en vanlig operasjon i mange programmeringsspråk som håndterer upresisjonen av flyttallsaritmetikk.

Elms tilnærming til avrunding er rett frem - funksjonene er rene og følger matematiske definisjoner for round, floor og ceiling. Elm forutser vanlige behov ved å tilby innebygde funksjoner, ettersom presisjonsstyring ofte er et krav, spesielt innen finans og grafikk.

Alternativer til Elms innebygde funksjoner kan inkludere egendefinerte implementeringer ved bruk av aritmetiske operasjoner, men det legger til unødvendig kompleksitet når standardbiblioteket allerede gjør jobben effektivt.

Per gjeldende versjon bruker Elm JavaScripts underliggende flyttallsmatematikk for disse operasjonene, dermed forblir konsistent med IEEE 754-standarden, noe som er verdt å huske på når man vurderer presisjon og potensielle flyttallsfeil.

## Se Også

- Elms offisielle `Basics`-modul dokumentasjon: https://package.elm-lang.org/packages/elm/core/latest/Basics
- En detaljert titt på hvordan flyttall fungerer i databehandling: https://floating-point-gui.de/
- Elm `Float`-modul for flere flyttallsoperasjoner: https://package.elm-lang.org/packages/elm/core/latest/Float
