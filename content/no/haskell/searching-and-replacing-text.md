---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Søk og erstatt tekst i Haskell

## Hva & Hvorfor?

Å søke og erstatte tekst handler om å finne spesifikke tegn eller sekvenser av tegn (strenger) i en tekst, og erstatte dem med en annen streng. Dette er en viktig operasjon som programmerere ofte må utføre for å manipulere og behandle data.

## Hvordan?

La oss se på et eksempel hvordan du kan implementere søk og erstatt funksjonalitet i Haskell:

```haskell
import Data.List.Utils

main = do
    let tekst = "Hei Verden"
    let erstattetTekst = replace "Verden" "Norge" tekst
    putStrLn erstattetTekst
```
Kjører du dette eksempelprogrammet, vil du se at det printer ut "Hei Norge". Funksjonen `replace` fra `Data.List.Utils` gjør hele jobben for oss.

## Dypdykk

Historisk sett, har søking og erstatting tekst vært en grunnleggende funksjon i tekstbehandling, det være seg i programmeringsspråk, tekstbehandlere, databaser, og selv regulære uttrykk (regex).

Det finnes en rekke alternativer til `Data.List.Utils.replace` i Haskell, som `Data.Text.replace` fra `Data.Text` biblioteket eller `subRegex` fra `Text.Regex` biblioteket for mer komplekse erstatninger.

Implementasjonsdetaljer for `Data.List.Utils.replace` er litt kompliserte. Den bruker en effektiv algoritme basert på lister for å finne og erstatte teksten, og er designet for å være både rask og minneeffektiv.

## Se også

Trenger du mer informasjon, kan du lese følgende:

* [Haskell Documentation: Data.List.Utils.replace](https://hackage.haskell.org/package/MissingH-1.4.3.0/docs/Data-List-Utils.html#v:replace)

* [Tutorial: Text Manipulation in Haskell](http://learnyouahaskell.com/input-and-output#text-processing)

Husk, den beste måten å lære på er å eksperimentere selv. God koding!