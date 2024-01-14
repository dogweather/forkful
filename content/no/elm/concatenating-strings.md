---
title:                "Elm: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor
En viktig del av å programmere er å kunne behandle tekst og data på en effektiv måte. En måte å gjøre dette på er gjennom å kombinere, eller konkatenere, ulike strenger. Dette kan være nyttig for å bygge mer komplekse strenger som skal brukes i kall til API-er eller utskrift til brukerinteraksjoner. I denne blogginnlegget vil vi se på hvordan man enkelt kan concatenate strenger i Elm.

## Hvordan gjøre det
I Elm er det veldig enkelt å concatenere strenger ved å bruke operatøren ++. Her er et eksempel på hvordan dette kan gjøres:

```Elm
navn = "Ola"
alder = "25"
beskjed = navn ++ " er " ++ alder ++ " år gammel."
```
Dette vil produsere en streng som sier "Ola er 25 år gammel". Som du kan se, bruker vi ++ operatøren for å kombinere variabler og strenger. Det er viktig å merke seg at operatøren kun fungerer med strenger, så om du ønsker å konvertere tall eller andre data typer må du først gjøre de om til strenger.

Du kan også concatenere strenger som er lagret i en liste. I følgende eksempel viser vi hvordan man kan bruke en map funksjon for å concatenate alle strengene i en liste:

```Elm
strenger = [ "Hei ", "på ", "deg!" ]
resultat = List.foldl (\streng akk -> akk ++ streng) "" strenger
```

Dette vil resultere i en streng som sier "Hei på deg!".

## Deep Dive
En ting å huske på når man concatenate strenger er at det også innebærer å håndtere mellomrom og formatering. Dette kan være spesielt viktig når man jobber med brukerinteraksjoner eller API-kall.

For å unngå uønskede mellomrom kan man bruke funksjonen String.join som vil concatenate strenger med et gitt mellomrom. For eksempel:

```Elm
strenger = [ "Det", "er", "kaldt", "ute" ]
resultat = String.join " " strenger
```

Dette vil resultere i en streng som sier "Det er kaldt ute".

Når man jobber med API-kall er det også viktig å formatere strenger riktig. For eksempel, hvis man skal bygge en URL som tar inn en variabel for å gjøre et søk, må man være sikker på at variabelen er riktig formatert. Dette kan enkelt gjøres ved hjelp av en concatenate funksjon som tar inn nødvendig formatering som et argument.

Generelt sett er det viktig å være klar over formatering og mellomrom når man jobber med concatenating strenger. Det kan være lurt å planlegge og teste grundig for å unngå uønskede resultater.

## Se også
- Elm Offisiell Dokumentasjon for String Modul: https://package.elm-lang.org/packages/elm/string/latest/
- Elm Offisiell Dokumentasjon for List Modul: https://package.elm-lang.org/packages/elm/core/latest/List
- Elm Bygge og Kombinere Strenger guide: https://guide.elm-lang.org/effects/string_conc