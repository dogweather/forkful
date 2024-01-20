---
title:                "Utdrag av understrenger"
html_title:           "Bash: Utdrag av understrenger"
simple_title:         "Utdrag av understrenger"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Utdrag av substrings er å hente en del av en streng basert på gitte indekser. Programmører gjør det for å manipulere og behandle data mer effektivt. 

## Hvordan gjør man det?

Du kan hente en substring i Elm ved hjelp av "String.slice" funksjonen. La oss se på følgende eksempel:

```elm
import String exposing (slice)

main =
    let
        bokstaver = "Hei, Verden!"
    in
        slice 0 3 bokstaver
```

Kjører du det, vil utfallet bli:

```elm
"Hei"
```

Vi ber om å hente en substring fra indeks 0 til 3. String.slice start indeksen er inklusiv mens slutt indeksen er eksklusiv. 

## Dyp Dykk

Substring teknikken stammer fra tidligere programmeringsspråk som C and C++, og blir ofte brukt når vi ønsker å hente ut en del av informasjonen i en større streng.

I Elm kan du også bruke funksjonen "String.dropLeft" eller "String.dropRight" for å hente substrings, men dette kan også innebære at funksjoner blir litt mer innviklet.

Elms implementering av substrings er støttet av både String modulen og List modulen for indeksbasert adgang. På grunn av dette kan du også hente ut substrings av lister og andre array-lignende strukturer.

## Se også  

Her er et par nyttige lenker for ytterligere læring:

1. [Elm's Offisielle Dokumentasjon på String.slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
2. [Real World Elm Eksempler](https://elmprogramming.com/)
3. [Elm's Offisielle Dokumentasjon på String modulet](https://package.elm-lang.org/packages/elm/core/latest/String)

Husk at mestring av grunnleggende string manipulasjon som substring extraksjon er en kritisk ferdighet for alle Elm utviklere.