---
title:    "Haskell: Å bruke regulære uttrykk"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor bør man bruke regulære uttrykk i Haskell-programmering? Regulære uttrykk er en kraftig verktøysett som lar deg søke og manipulere tekst på en effektiv måte. Dette kan være svært nyttig når du jobber med tekstbaserte data, som for eksempel loggfiler eller tekstfiler.

# Hvordan

For å bruke regulære uttrykk i Haskell, må du importere modulet "Text.Regex.Posix". Deretter kan du bruke funksjonen `match` til å søke etter et mønster i en tekststreng. Her er et eksempel på hvordan du kan finne alle forekomster av et tall i en tekststreng:

```Haskell
import Text.Regex.Posix

tekst = "Jeg er 27 år gammel og har jobbet som programmerer i 4 år."
match "([0-9]+)" tekst
```

Dette vil returnere en liste med matchende substringer, i dette tilfellet "27" og "4". Du kan også bruke regulære uttrykk til å erstatte deler av en tekststreng. For eksempel kan du endre alle forekomster av bokstaven "e" til "a":

```Haskell
tekst2 = "Dette er en test"
subRegex (mkRegex "e") tekst2 "a"
```

Dette vil gi deg resultatet "Datta ar an tast".

# Dypere dykk

Regulære uttrykk følger et spesielt syntaks, og det kan ta litt tid å bli kjent med den. Det finnes mange ulike tegn og operatorer som kan brukes til å lage mønstre, og det er viktig å forstå hvordan de påvirker søket ditt. Det finnes også avanserte funksjoner, som for eksempel å bruke "grupper" for å hente ut spesifikke deler av en matches tekststreng.

En annen viktig ting å merke seg når du bruker regulære uttrykk, er at de kan være svært effektive ved riktig bruk, men også veldig ressurskrevende hvis de brukes feil. Det kan derfor være lurt å teste og optimalisere uttrykkene dine for å få bedre ytelse.

# Se også

- [Offisiell Haskell-dokumentasjon om regulære uttrykk](https://www.haskell.org/hoogle/?q=Text.Regex.Posix)
- [Nyttig nettsted for å teste og øve på regulære uttrykk](https://regexr.com/)
- [Mer avanserte eksempler på bruk av regulære uttrykk i Haskell](https://wiki.haskell.org/Regular_expressions)