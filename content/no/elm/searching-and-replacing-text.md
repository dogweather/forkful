---
title:                "Søking og erstattning av tekst"
html_title:           "Elm: Søking og erstattning av tekst"
simple_title:         "Søking og erstattning av tekst"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor
Vi har alle vært der - sitter og redigerer en kodebase og plutselig innser at vi trenger å endre et bestemt ord eller uttrykk som forekommer flere steder. I stedet for å manuelt gå gjennom hver fil og endre det, kan man spare tid og unngå potensielle feil ved å bruke søk og erstatte funksjonaliteten til Elm.

## Hvordan
Søke og erstatte i Elm er enkelt og intuitivt. Her er et eksempel på hvordan man kan erstatte alle forekomster av "hund" med "katt" i en liste:

```Elm
listeAvDyr = ["hund", "giraff", "elefant", "hund"]

List.map (\dyr -> 
    if dyr == "hund" then 
        "katt" 
    else 
        dyr
    ) listeAvDyr
```

Output vil da bli `["katt", "giraff", "elefant", "katt"]`. Her bruker vi `List.map` funksjonen til å iterere gjennom hver verdi i listen og sjekke om den er lik "hund". Hvis den er det, erstatter vi den med "katt", ellers beholder vi den originale verdien.

Det er også mulig å bruke søk og erstatte på tekststrenger. For å bytte ut den første forekomsten av "hund" med "katt" i en tekststreng kan man bruke `String.replaceFirst`:

```Elm
tekst = "Hunden min liker å gå tur med hunden til naboen"

String.replaceFirst "hund" "katt" tekst
```

Output vil da bli "Katten min liker å gå tur med hunden til naboen". Dette er bare noen få eksempler på hvordan man kan bruke søk og erstatte funksjonaliteten i Elm.

## Dypdykk
Søke og erstatte funksjonaliteten i Elm er ikke bare begrenset til ting som er enkle å finne og erstatte. Man kan også bruke regulære uttrykk for mer avansert søking og erstatting. Elm har innebygd støtte for regulære uttrykk gjennom `Regex` biblioteket.

Her er et eksempel på hvordan man kan bruke regulære uttrykk til å finne og erstatte alle tall i en tekststreng med "X":

```Elm
import Regex exposing (replace, regex)

tekst = "Jeg kjøpte 123 appelsiner og 45 bananer"

replace (regex "\\d+") (\_ -> "X") tekst
```

Output vil da bli "Jeg kjøpte X appelsiner og X bananer". Her bruker vi `Regex.replace` funksjonen til å finne alle tall i teksten og erstatte dem med bokstaven "X".

For mer informasjon om regulære uttrykk i Elm, se dokumentasjonen for `Regex` biblioteket.

## Se også
- [Offisiell Elm Dokumentasjon](https://guide.elm-lang.org)
- [Elm Forum](https://discourse.elm-lang.org)
- [Awesome Elm - Samling av ressurser og verktøy for Elm](https://github.com/sporto/awesome-elm)