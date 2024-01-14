---
title:    "Haskell: Sammenføyning av strenger"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å slå sammen strenger, eller "concatenate strings" som det kalles på engelsk, er en viktig del av programmering i Haskell. Det lar deg kombinere flere strenger til en enkelt streng, noe som kan være nyttig for å formatere tekst eller bygge dynamiske meldinger.

## Hvordan gjøre det

Det første du må gjøre er å importere "Data.List" biblioteket i Haskell-koden din. Dette vil gi deg tilgang til "concat" funksjonen, som lar deg slå sammen strenger.

```Haskell
import Data.List

concat "Hei på deg" "Hvordan går det?" 
```
Output vil være "Hei, på deg Hvordan går det?"

Hvis du vil legge til flere strenger, kan du bruke en liste med strenger og bruke "concat" på den. Her er et eksempel hvor vi slår sammen tre navn fra en liste:

```Haskell
navn = ["Per", "Pål", "Espen"]

concat navn
```

Output vil være "PerPålEspen".

## Gå i dybden

I Haskell er strenger bare lister av tegn, så "concat" funksjonen fungerer ved å kombinere disse listene til en enkelt liste. Dette gjør det også mulig å bruke "concat" for å legge til enkelttegn i en streng.

```Haskell
concat ['H', 'a', 's', 'k', 'e', 'l', 'l']
```

Output vil være "Haskell".

Det er også verdt å nevne at i Haskell er strenger "immutable", som betyr at de ikke kan endres etter at de er opprettet. Så når du slår sammen strenger, lager du egentlig en ny streng istedenfor å endre den opprinnelige.

## Se også

- [Haskell-tutorials på norsk](https://haskell-tutorials-no.readthedocs.io/nn/latest/index.html)
- [Offisiell Haskell-dokumentasjon](https://www.haskell.org/documentation/)