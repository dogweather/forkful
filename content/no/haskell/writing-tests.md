---
title:                "Skrive tester"
html_title:           "Haskell: Skrive tester"
simple_title:         "Skrive tester"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive tester er en viktig del av å være en god utvikler. Det sikrer at koden vår fungerer som den skal, og gjør det enklere å identifisere og fikse feil.

## Hvordan Skrive Tester i Haskell

Det første vi må gjøre er å importere testbiblioteket HUnit ved å skrive følgende linje øverst i koden vår:

```Haskell
import Test.HUnit
```

Deretter kan vi begynne å skrive vårt første test ved å bruke funksjonen `TestLabel`. Her er et eksempel på en enkel testfunksjon som sjekker om 2+2 er lik 4:

```Haskell
test1 = TestCase (assertEqual "2+2 er lik 4" 4 (2+2))
```

Vi kan også gruppere flere tester sammen ved å bruke funksjonen `TestList`, som lar oss liste opp flere testfunksjoner. Her er et eksempel på en testliste med to tester:

```Haskell
tests = TestList [test1, test2]
```

For å kjøre testene våre, bruker vi funksjonen `runTestTT` og gir den testlisten som parameter:

```Haskell
main = do
    runTestTT tests
```

Output fra dette programmet vil vise oss om testene våre har passert eller feilet. Hvis vi ønsker å sjekke om en funksjon returnerer riktig verdi, kan vi bruke funksjonen `assertEqual` til å sammenligne verdien med forventet resultat.

## Dypdykk

En av de største fordelene med å skrive tester er at det hjelper oss med å identifisere og fikse feil i koden vår. Ved å skrive tester før vi implementerer funksjonalitet, tvinger det oss til å tenke på alle mulige tilfeller og sikre at koden vår håndterer dem på en ønsket måte.

Det finnes også flere testrammeverk i Haskell, som QuickCheck og SmallCheck, som lar oss generere tilfeldige tester for å sjekke om koden vår tåler forskjellige input. Dette kan være spesielt nyttig når vi koder med typer og ønsker å være sikre på at koden vår håndterer alle mulige typer input.

## Se Også

- [Offisiell HUnit Dokumentasjon](https://hackage.haskell.org/package/HUnit)
- [Gentesting med QuickCheck](https://www.haskell.org/tutorial/generating.html)
- [Haskell Programmeringsspråk](https://www.haskell.org/)