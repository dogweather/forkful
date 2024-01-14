---
title:                "Haskell: Skriving til standardfeil"
simple_title:         "Skriving til standardfeil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man skriver kode i Haskell, er det viktig å kunne håndtere feil og unntak på en effektiv måte. En god måte å gjøre dette på er å skrive til standard error (stderr) i stedet for standard output (stdout). Dette lar våre brukere få informasjon om feil og unntak som påvirker programmets utførelse.

## Hvordan

For å skrive til stderr i Haskell, må vi bruke "System.IO" biblioteket og importere "stderr" funksjonen. Deretter kan vi kalle denne funksjonen og gi den en melding som parameter. Her er et enkelt eksempel som viser hvordan man skriver en enkel feilmelding til stderr:

```Haskell
import System.IO

main = hPutStrLn stderr "En feil har oppstått i programmet."
```

Når vi kjører dette programmet, vil vi se at meldingen blir skrevet til stderr i stedet for stdout.

## Dypdykk

Det finnes også en rekke andre funksjoner og metoder for å håndtere stderr i Haskell. For eksempel kan vi bruke "System.IO.hPutStrLn" funksjonen til å skrive en melding til stderr, men denne gangen med mulighet for å formatere meldingen med argumenter. Her er et eksempel som viser dette:

```Haskell
import System.IO

main = do
  let num = 100
  hPutStrLn stderr $ "En feil med nummer " ++ show num ++ " har oppstått."
```

I dette tilfellet bruker vi "show" funksjonen for å konvertere tallet vår til en streng, slik at vi kan inkludere det i meldingen.

## Se Også

- [System.IO.stderr dokumentasjon](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:stderr)
- [hPutStrLn dokumentasjon](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:hPutStrLn)