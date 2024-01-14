---
title:                "Gleam: Å sjekke om en mappe eksisterer"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Du lurer kanskje på hvorfor du bør sjekke om en mappe eksisterer i Gleam-programmering. Å sjekke om en mappe eksisterer kan være nyttig når du for eksempel vil lese eller skrive til en fil, men først må du vite om mappen du ønsker å bruke faktisk finnes.

## Hvordan gjøre det

For å sjekke om en mappe eksisterer i Gleam, kan du bruke funksjonen `exists` fra standardbiblioteket `os`. Denne funksjonen tar inn en streng som representerer mappenavnet, og returnerer en boolean-verdi som indikerer om mappen eksisterer eller ikke.

```Gleam
let mappe_navn = "min_mappe"
let eksisterer = os.exists(mappe_navn)
```

Hvis eksisterer er `true`, betyr det at mappen allerede finnes. Hvis den er `false`, må du kanskje lage en ny mappe med samme navn før du kan lese eller skrive til den.

```Gleam
if eksisterer {
  // Les eller skriv til mappen som allerede finnes
} else {
  // Lag en ny mappe med samme navn
}
```

## Dypdykk

Det kan være lurt å være forsiktig når du sjekker om en mappe eksisterer i Gleam. Hvis du for eksempel bruker `exists` på en mappe som ikke finnes, vil den returnere `false`, men hvis du prøver å lese eller skrive til denne mappen senere, vil det resultere i en feil.

En annen ting å huske på er at `exists` sjekker om mappen eksisterer akkurat i det øyeblikket funksjonen blir kalt. Hvis noen oppretter eller sletter mappen samtidig som programmet ditt kjører, kan det føre til uønsket oppførsel.

## Se også

- [Dokumentasjon for `os`-biblioteket](https://gleam.run/core/gleam-os.html#exists)
- [Guide til å lage mapper i Gleam](https://gleam.run/blog/creating-directories-in-gleam.html)