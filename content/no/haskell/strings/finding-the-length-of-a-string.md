---
date: 2024-01-20 17:47:38.968317-07:00
description: "Slik gj\xF8r du: Haskell gj\xF8r det enkelt \xE5 finne en strengs lengde\
  \ med `length`-funksjonen. Her er et eksempel."
lastmod: '2024-03-13T22:44:40.832027-06:00'
model: gpt-4-1106-preview
summary: "Haskell gj\xF8r det enkelt \xE5 finne en strengs lengde med `length`-funksjonen."
title: "Finn lengden p\xE5 en streng"
weight: 7
---

## Slik gjør du:
Haskell gjør det enkelt å finne en strengs lengde med `length`-funksjonen. Her er et eksempel:

```haskell
main :: IO ()
main = do
    let myString = "Hallo, Norge!"
    print $ length myString
```

Forventet output:

```
12
```

## Dypdykk
Historisk sett har streng-operasjoner vært grunnleggende i programmering, og funksjonen `length` har vært med siden de første versjonene av Haskell. `length` teller antall elementer i en liste, og siden en streng i Haskell er en liste av tegn, teller `length` antall tegn. Alternativt kan man bruke `Data.Text`-pakken for bedre ytelse med store strenger. Implementasjonsdetaljer å tenke på inkluderer effektivitet—`length` opererer i O(n) tid, så for svært lange lister kan dette påvirke ytelse.

## Se Også
- [Haskell Documentation on `length`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:length)
- [Performance impact with `Data.Text`](https://hackage.haskell.org/package/text-1.2.5.0/docs/Data-Text.html)
