---
title:    "Elm: Å finne lengden av en streng"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Å finne lengden på en streng er en viktig og vanlig oppgave i programmering. Det lar deg manipulere tekst og behandle tekstdata på en enkel måte. Dette kan være nyttig for å lage en funksjon som teller antall tegn i et essay eller en app som begrenser antall tegn i et tekstfelt.

## Hvordan
Vi kan bruke Elm til enkelt å finne lengden på en streng. Her er et eksempel på hvordan du kan gjøre det:
```Elm
lengthOf "Hei, verden!" 
-- Output: 12
```
Her bruker vi funksjonen `lengthOf` som tar inn en streng som argument og returnerer lengden på strengen som et heltall. Det er så enkelt som det!

## Dypdykk
Det er verdt å merke seg at `lengthOf` funksjonen i Elm ikke bare fungerer for vanlige strenger, men også for lister, som for eksempel tall og boolske verdier. Dette betyr at du kan finne lengden på enhver type liste, ikke bare tekststrenger.

En annen ting å huske på er at `lengthOf` funksjonen er en del av `String` modulen i Elm. Dette betyr at du må importere denne modulen i koden din før du kan bruke `lengthOf` funksjonen.

## Se også
- [Offisiell Elm dokumentasjon for lengdefunksjonen](https://elm-lang.org/docs/string#length)
- [Codecademy: Tilgang til lengden av en streng i Elm](https://www.codecademy.com/learn/learn-elm/modules/elm-strings/cheatsheet)
- [Utvikler blogg: Enkel håndtering av tekst i Elm](https://blog.logrocket.com/handling-text-in-elm/)