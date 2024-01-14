---
title:    "Haskell: Utskrift av feilsøkingsutdata"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

# Hvorfor

Det kan være frustrerende å feilsøke koden din og finne ut hva som egentlig skjer. Dette er spesielt viktig når man jobber med Haskell, et funksjonelt programmeringsspråk som kan være krevende å forstå. Å skrive ut feilmeldinger og annen debug-informasjon kan være en avgjørende hjelp for å løse problemer og forstå hvordan koden din fungerer i detalj.

# Hvordan

Det å skrive ut debug-informasjon kan gjøres på flere forskjellige måter i Haskell. En av de enkleste måtene er å bruke funksjonen `print` som tar inn en verdi og skriver den ut til konsollen. Her er et eksempel på hvordan man kan skrive ut en liste med tall i Haskell:

```Haskell
main = do
    let numbers = [1, 3, 5, 7, 9]
    print numbers
```

Output vil da være:

```
[1, 3, 5, 7, 9]
```

En annen måte å skrive ut debug-informasjon på er å bruke funksjonen `trace` fra pakken `Debug.Trace`. Denne funksjonen tar inn en streng og en verdi, og skriver begge til konsollen. Et eksempel på hvordan dette kan brukes i praksis er:

```Haskell
import Debug.Trace

calculateSum :: Int -> Int -> Int
calculateSum x y = trace ("x + y is " ++ show (x + y)) (x + y)
```

Nå vil funksjonen `calculateSum` skrive ut en melding til konsollen som viser hva summen av to tall er.

# Dypdykk

Hvis du vil få mer detaljert debug-informasjon, kan du bruke funksjonen `Debug.Trace.traceShow` som tar inn en streng og en verdi, og skriver dem begge til konsollen sammen med informasjon om data-typen til verdien. Dette er spesielt nyttig når man jobber med komplekse datastrukturer, som for eksempel lister av lister eller trær.

En viktig ting å huske på når man bruker debug-informasjon er å fjerne eller kommentere ut denne koden før man leverer den endelige versjonen av programmet sitt. Ellers vil det bli skrevet ut mye unødvendig informasjon til konsollen som kan gjøre programmet ditt tregere.

# Se også

- [Offisiell Haskell-dokumentasjon](https://www.haskell.org/documentation)
- [Haskell for nybegynnere](https://medium.com/@noemiandor/haskell-for-beginners-9e8d06c0e5a8)
- [Haskell-programmeringsspråket](https://no.wikipedia.org/wiki/Haskell_(programmeringsspr%C3%A5k))