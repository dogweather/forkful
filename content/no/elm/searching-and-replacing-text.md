---
title:    "Elm: Søke og erstatte tekst"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Vi har alle vært der - du skriver en kode i Elm, og plutselig innser du at du må gjøre endringer i store deler av koden din. Å gå gjennom hver eneste linje og gjøre endringene manuelt kan være både tidkrevende og ineffektivt. Derfor er det viktig å bli kjent med søk og bytte-funksjonen i Elm, som gjør det enkelt å finne og erstatte tekst i en kodebase.

## Hvordan

For å søke og erstatte tekst i Elm, kan du bruke "String.replace" funksjonen. Denne tar inn tre argumenter - en tekst du vil søke etter, en tekst du vil erstatte den med, og den opprinnelige teksten du søker i. Her er et eksempel:

```Elm
String.replace "Hund" "Katt" "Jeg liker å gå tur med hunden min."
```

Dette vil returnere teksten "Jeg liker å gå tur med katten min." som output. Hvis du vil erstatte all forekomst av en tekst, kan du legge til et fjerde argument som sier hvor mange forekomster du vil erstatte. For å erstatte alle forekomster, kan du bruke -1 som et argument. 

For å søke og erstatte i en liste med tekster, kan du bruke funksjonen "List.map" sammen med "String.replace" funksjonen. Dette vil gjøre søket og erstattingen på hver enkelt tekst i listen. Her er et eksempel:

```Elm
myListOfTexts = ["Jeg liker å spise bananer", "Jeg liker å drikke melk"]
List.map (String.replace "Jeg" "Du") myListOfTexts
```

Dette vil returnere en liste med tekstene "Du liker å spise bananer" og "Du liker å drikke melk".

## Dypdykk

Det er viktig å være oppmerksom på at "String.replace" funksjonen i Elm er case-sensitive, noe som betyr at den skiller mellom store og små bokstaver. Dette kan føre til at teksten du leter etter ikke blir funnet hvis det er en forskjell i store og små bokstaver. For å unngå dette kan du bruke "String.toLower" funksjonen for å konvertere teksten til små bokstaver før du søker og erstatter. 

En annen nyttig funksjon i Elm er "String.replaceRegex", som lar deg søke og erstatte tekst basert på et regex-mønster. Dette kan være svært nyttig for mer avanserte søk og erstatting. 

## Se også

- [Offisiell dokumentasjon for String-modulen i Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Eksempler på bruk av String.replace funksjonen i Elm](https://elmprogramming.com/string-manipulation.html#replace)