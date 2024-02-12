---
title:                "Finn lengden på en streng"
aliases:
- /no/haskell/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:38.968317-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finn lengden på en streng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å finne lengden på en streng betyr ganske enkelt å telle antall tegn i den. Vi gjør dette for å validere input, begrense tekst, og jobbe effektivt med data.

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
