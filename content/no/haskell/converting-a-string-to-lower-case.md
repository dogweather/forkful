---
title:                "Konvertering av en streng til små bokstaver"
html_title:           "Haskell: Konvertering av en streng til små bokstaver"
simple_title:         "Konvertering av en streng til små bokstaver"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av en streng til små bokstaver er å endre alle bokstavene i en tekststreng til små bokstaver. Det er nyttig for å standardisere og forenkle tekstbehandling og sammenligning av strenger. Programmere gjør dette for å sikre lik behandling av tekst og unngå unødvendige feil.

## Hvordan:
Kodeeksempler og sample output:

```Haskell
import Data.Char (toLower)

lowerCaseString :: String -> String
lowerCaseString s = map toLower s

lowerCaseString "HeLlO WoRlD"  -- output: "hello world"
```

En annen måte å konvertere en streng til små bokstaver er å bruke funksjonen `map` sammen med `Data.Etafunktor` modulen:

```Haskell
import Data.Etafunktor (fmap)
import Data.Char (toLower)

lowerCaseString :: String -> String
lowerCaseString s = fmap toLower s

lowerCaseString "HaVe A nIcE DaY"  -- output: "have a nice day"
```

## Dypere dykk:
Historisk kontekst:
Konvertering av en streng til små bokstaver har vært en viktig del av tekstbehandling siden den første datamaskinen ble oppfunnet. I eldre programmeringsspråk som C og Fortran måtte programmene selv håndtere konverteringen. Men med framveksten av funksjonelt programmeringsspråk som Haskell, er det nå innebygd funksjonalitet for å gjøre dette enkelt og effektivt.

Alternative tilnærminger:
I tillegg til `toLower` funksjonen som vi brukte i eksemplene, tilbyr Haskell også `toLower` funksjonen som er spesifikt for enkelte språk og skriftsystemer. For eksempel `toLower_greek` og `toLower_hebrew` for henholdsvis gresk og hebraisk. Dette gjør det enkelt å håndtere spesifikke språk i tekstbehandling.

Implementasjonsdetaljer:
Kjernen av konvertering av en streng til små bokstaver er å endre ASCII-kodene til de respektive bokstavene. Dette gjøres ved hjelp av matematiske operasjoner og funksjoner som er innebygd i Haskell.

## Se også:
- [Haskell.org](https://www.haskell.org/) - Offisiell hjemmeside for Haskell språket.
- [Haskell wiki](https://wiki.haskell.org/) - Wiki med informasjon om Haskell.
- [Hackage](https://hackage.haskell.org/) - Sentralisert bibliotek for Haskell pakker.