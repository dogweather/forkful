---
title:                "Haskell: Fjerning av tegn som matcher et mønster"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster kan være nyttig når du ønsker å filtrere ut uønskede tegn i en tekststreng. Dette kan for eksempel være nyttig når du jobber med dataanalyse eller tekstbehandling.

## Hvordan gjøre det

Det finnes flere måter å slette tegn som matcher et mønster på i Haskell. Du kan bruke funksjonen `filter` til å filtrere ut tegn basert på et gitt kriterium. For eksempel, hvis du vil slette alle tall fra en tekststreng, kan du bruke følgende kode:

```Haskell
filter (\x -> not $ isDigit x) "He1llo Wor2ld!" -- output: "Hello World!"
```

Her bruker vi funksjonen `filter` sammen med en anonym funksjon som sjekker om hvert tegn ikke er et tall ved hjelp av funksjonen `isDigit` fra standardbiblioteket `Data.Char`. Vi bruker også funksjonen `not` for å invertere resultatet, slik at `filter` returnerer alle tegn som ikke er tall.

En annen måte å gjøre det på er å bruke funksjonen `delete` fra `Data.List`-biblioteket. Denne funksjonen tar inn et tegn og en tekststreng, og sletter alle forekomster av det gitte tegnet fra teksten. For eksempel:

```Haskell
delete 'l' "Hello World!" -- output: "Heo Word!"
```

Husk at disse eksemplene kun er ment som illustrasjoner, og du kan tilpasse koden etter behov, avhengig av hvilke tegn du ønsker å slette og hvordan.

## Dypdykk

For å forstå hvordan disse funksjonene fungerer, kan det være nyttig å se nærmere på hvordan de er implementert. `filter`-funksjonen bruker en teknikk som kalles rekursjon, som går ut på å bruke en funksjon på et mindre og mindre sett av data til den når et basistilfelle og returnerer et resultat. I tilfellet med å slette tegn som matcher et mønster, vil funksjonen kalle seg selv på hvert tegn i teksten og returnere en ny tekststreng uten de uønskede tegnene.

`delete`-funksjonen bruker en litt annen tilnærming og starter med å sjekke om det første tegnet i teksten matcher det gitte tegnet. Hvis det gjør det, returneres resten av teksten uten det første tegnet. Hvis det første tegnet ikke matcher, kaller funksjonen seg selv på resten av teksten og returnerer resultatet med det første tegnet lagt tilbake.

## Se også

- [Filtering in Haskell](https://wiki.haskell.org/Filtering)
- [Delete function in Haskell](https://www.geeksforgeeks.org/haskell-delete-function/)
- [Data.List documentation](https://www.haskell.org/hoogle/?hoogle=Data.List#v:delete)