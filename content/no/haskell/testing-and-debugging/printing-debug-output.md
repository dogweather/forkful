---
date: 2024-01-20 17:52:39.063029-07:00
description: "Hvordan: Historisk sett har utskrift til konsoll v\xE6rt en enkel m\xE5\
  te \xE5 spore programflyt og variabeltilstander. I Haskell, der renhet og frav\xE6\
  ret av\u2026"
lastmod: '2024-04-05T22:50:54.851619-06:00'
model: gpt-4-1106-preview
summary: "Historisk sett har utskrift til konsoll v\xE6rt en enkel m\xE5te \xE5 spore\
  \ programflyt og variabeltilstander."
title: "Skrive ut feils\xF8kingsdata"
weight: 33
---

## Hvordan:
```Haskell
main :: IO ()
main = do
    putStrLn "Debug: Starter programmet"
    let x = 42
    print x -- Skriver ut verdien av x
    putStrLn "Debug: Programmet avsluttes"
```

Eksempel på output:
```
Debug: Starter programmet
42
Debug: Programmet avsluttes
```

## Dypdykk:
Historisk sett har utskrift til konsoll vært en enkel måte å spore programflyt og variabeltilstander. I Haskell, der renhet og fraværet av sideeffekter verdsettes, brukes `IO` monaden for å håndtere utskrifter. Det finnes alternativer til standard utskrift, som `Debug.Trace`, men disse går på akkord med renhet og bør brukes med forsiktighet.

## Se Også:
- Haskell dokumentasjon om IO: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#g:28
- `Debug.Trace` modulen: https://hackage.haskell.org/package/base-4.16.1.0/docs/Debug-Trace.html
- Blogg om feilsøking i Haskell: https://www.fpcomplete.com/haskell/tutorial/debugging/
