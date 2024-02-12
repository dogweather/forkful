---
title:                "Skrive ut feilsøkingsdata"
aliases: - /no/haskell/printing-debug-output.md
date:                  2024-01-20T17:52:39.063029-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
"Printing debug output" betyr å vise mellomliggende data fra et program for å forstå hva som skjer under kjøring. Programmerere gjør dette for å feilsøke og finjustere kode.

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
