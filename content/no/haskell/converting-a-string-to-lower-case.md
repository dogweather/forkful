---
title:                "Haskell: Konvertere en streng til små bokstaver"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor

Mange programmerere ønsker å konvertere en streng til små bokstaver av ulike årsaker. Noen ganger kan det være for å sammenligne tekster uten hensyn til store og små bokstaver, eller for å få en mer konsistent og ensartet formatering i en applikasjon. Uansett årsak, er det nyttig å kunne konvertere en streng til små bokstaver i Haskell.

## Hvordan Du Gjør Det

Det er flere måter å konvertere en streng til små bokstaver i Haskell, men en av de enkleste metodene er ved å bruke funksjonen "map" sammen med funksjonen "toLower". Her er et eksempel på hvordan du kan gjøre det:

```Haskell
import Data.Char (toLower) 
map toLower "Norsk"
```

Dette resulterer i en output på "norsk", med alle bokstavene i den opprinnelige strengen konvertert til små bokstaver. Du kan også bruke funksjonen "toLower" på en liste av tegn, som vist i dette eksemplet:

```Haskell
map toLower "Hei! Hvordan går det?"
```

Output her vil være "hei! hvordan går det?".

En annen måte å konvertere en streng til små bokstaver på er ved å bruke funksjonen "map" og "toLower" sammen med funksjonen "lines", som separerer strengen basert på linjeskift. Her er et eksempel på hvordan du gjør det:

```Haskell
import Data.Char (toLower)
map (map toLower . line) "Hei! Hvordan går det?\nBra, takk."
```

I dette tilfellet vil output være en liste av linjer hvor hver bokstav er konvertert til små bokstaver.

## Dypdykk

I Haskell er strenger egentlig bare lister av tegn. Derfor kan du bruke alle funksjoner og metoder som er tilgjengelige for lister på strenger. For eksempel kan du bruke "head" funksjonen for å få den første bokstaven i en streng, eller "length" funksjonen for å finne lengden på en streng.

Det finnes også andre metoder for å konvertere strenger til små bokstaver i Haskell, som å bruke funksjonen "toLower" på individuelle tegn ved hjelp av "map" funksjonen. Det er også mulig å bruke biblioteket "Data.Text" for mer effektive operasjoner på tekster.

## Se Også

[Data.Char dokumentasjon](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)

[Haskell Tutorial: Strings](https://www.haskell.org/tutorial/strings.html)

[Haskell Formatting Strings](https://stackoverflow.com/questions/3128517/haskell-formatting-strings)