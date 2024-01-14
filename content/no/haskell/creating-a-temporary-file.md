---
title:                "Haskell: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer er en vanlig oppgave i programmering, spesielt i tilfeller der det er behov for å lagre midlertidig data eller kommunisere med eksterne applikasjoner. Det kan også være nyttig når du ønsker å teste en funksjon eller metode uten å påvirke den eksisterende koden.

## Hvordan

For å opprette en midlertidig fil i Haskell, kan du bruke funksjonen "withSystemTempFile" fra pakken "System.IO.Temp". Dette vil opprette en midlertidig fil i det midlertidige mappen på datamaskinen din, og returnere filstien og en håndterer for å skrive til filen. Her er et eksempel:

```Haskell

import System.IO.Temp (withSystemTempFile)

main = do
  withSystemTempFile "example.txt" $ \filePath handle -> do
    hPutStrLn handle "Dette er en midlertidig fil."
    hClose handle
    putStrLn $ "Filopprettelse vellykket: " ++ filePath
    
```

Dette eksempelet vil opprette en midlertidig fil med navnet "example.txt" og skrive teksten "Dette er en midlertidig fil." til filen. Deretter vil det skrive ut en melding som bekrefter at filen ble opprettet.

## Dypdykk

I eksempelet ovenfor brukes funksjonen "withSystemTempFile" til å håndtere både opprettelsen og håndteringen av den midlertidige filen. Denne funksjonen sørger også for at filen blir slettet når programmet avsluttes, noe som er spesielt nyttig for å unngå overflødig rot på datamaskinen.

Haskell har også andre funksjoner som kan brukes til å opprette og håndtere midlertidige filer, som for eksempel "withTempFile" og "withTempDirectory". Disse funksjonene gir mer kontroll over navn og plassering av den midlertidige filen, men krever også at man manuelt sletter filen når den ikke lenger er nødvendig.

## Se Også

- [System.IO.Temp dokumentasjon](https://hackage.haskell.org/package/temp-1.3.1.1/docs/System-IO-Temp.html)
- [Haskell-tempeksperiment på GitHub](https://github.com/haskell/haskell-tempeksperiment)
- [Tutorial: Filhåndtering i Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/11-File-Handling)