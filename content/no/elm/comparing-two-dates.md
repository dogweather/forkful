---
title:    "Elm: Sammenligning av to datoer"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sammenligne to datoer er en vanlig utfordring når man jobber med programmering. Enten det er å sjekke om en bestemt hendelse har skjedd før eller etter en annen, eller å sortere en liste med datoer, er det viktig å kunne sammenligne dem på en nøyaktig og effektiv måte.

# Hvordan

For å sammenligne to datoer i Elm, kan vi bruke `Date.compare` funksjonen. Denne funksjonen tar inn to `Date` verdier og returnerer en `Order` som kan være `Less`, `Equal` eller `Greater`, avhengig av forholdet mellom de to datoene.

La oss se på et eksempel der vi sammenligner to datoer og deretter sjekker resultatet:

```elm
myDate1 = Date.fromString "2020-01-01"
myDate2 = Date.fromString "2020-05-05"

resultat = Date.compare myDate1 myDate2 

```

I dette tilfellet vil `resultat` variabelen være satt til `Less`, siden `myDate1` kommer før `myDate2` i kalenderen. Vi kan også bruke denne funksjonen til å sortere en liste med datoer, i stedet for å bruke en innebygd funksjon som `List.sortBy`.

# Dypdykk

Når man sammenligner datoer, er det viktig å være klar over at det ikke bare er dagene som sammenlignes, men også tidspunktene. Dette kan føre til uventede resultater, spesielt når man jobber med datoer fra forskjellige tidszoner.

En annen ting å være oppmerksom på er at `Date.fromString` funksjonen forventer datoer i formatet "yyyy-mm-dd", men hvis man får datoer i et annet format, kan det føre til feil ved sammenligning. Det er derfor viktig å passe på at datoene som sammenlignes er i samme format.

# Se også

- Elm Date Package (https://package.elm-lang.org/packages/elm/time/latest/)
- Elm Documentation on Date (https://package.elm-lang.org/packages/elm/time/latest/Date)
- Date Comparison in JavaScript vs Elm (https://dev.to/derekbeattie/date-comparison-in-javascript-vs-elm-46df)