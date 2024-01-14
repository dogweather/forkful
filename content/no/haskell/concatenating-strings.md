---
title:    "Haskell: Sammenføyning av strenger"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

Det å kombinere strenger er en grunnleggende, men likevel viktig del av Haskell-programmering. Ved å slå sammen to eller flere strenger, kan man lage nye, mer komplekse uttrykk som er nyttige i mange forskjellige programmeringsprosjekter.

## Hvorfor

Man kan ha mange grunner til å engasjere seg i å kombinere strenger i Haskell. Kanskje ønsker man å lage et program som genererer tekstbaserte meldinger basert på ulike variabler, eller kanskje trenger man å sette sammen ulike deler av en URL. Uansett hva motivasjonen er, er kunnskap om hvordan man konkatenere strenger avgjørende for å kunne utnytte Haskell til sitt fulle potensial.

## Slik gjør du det

I Haskell finnes det flere måter å konkatenere strenger på. Den mest grunnleggende metoden er å bruke operatora `++`, som lager en ny streng ved å sette sammen to eksisterende strenger. For eksempel:

```Haskell
"Hello " ++ "World"  -- Resultat: "Hello World"
```

Man kan også bruke funksjonen `concat` for å konkatenere en liste av strenger. Dette er nyttig hvis man ønsker å sette sammen flere strenger i en lang liste. Et annet alternativ er å bruke funksjonen `intercalate`, som tar en streng som argument og setter sammen en liste av strenger med denne som separator. Her er et eksempel på begge disse metodene:

```Haskell
concat ["Hello ", "World"]  -- Resultat: "Hello World"
intercalate ", " ["Hello", "World", "Haskell"]  -- Resultat: "Hello, World, Haskell"
```

Det finnes også egne funksjoner for å slå sammen strenger med mellomrom (`unwords`) og linjeskift (`unlines`).

## Dykk dypere

Når man konkatenere strenger i Haskell, er det viktig å være oppmerksom på at strenger håndteres på en litt annen måte enn andre datatype. Haskell bruker en strøm av laziness, som betyr at verdier ikke beregnes før de blir brukt. Dette gjelder også for strenger, som blir lagret som en liste av Unicode-tegn. Dette har konsekvenser når man kombinerer strenger, da dette kan føre til uventede resultat hvis man ikke er oppmerksom på hvordan Haskell håndterer strenger internt.

## Se også

* [Offisiell dokumentasjon for strenger i Haskell](https://www.haskell.org/tutorial/strings.html)
* [Leksjoner i Haskell fra Universitetet i Oslo](http://folk.uio.no/johangso/haskell/leksjoner/)
* [Haskell-programmeringsmiljø i Norge](https://www.haskell.no/)