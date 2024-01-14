---
title:    "Haskell: Å lese kommandolinje-argumenter"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor lese kommandolinjeargumenter i Haskell?

Noen ganger vil du kanskje at Haskell-programmet ditt skal kunne ta imot argumenter direkte fra kommandolinjen. Dette kan være nyttig hvis du ønsker å kjøre det samme programmet flere ganger med forskjellige inputverdier, eller hvis du trenger å spesifisere bestemte innstillinger for programmet ditt.

## Slik gjør du det

For å lese kommandolinjeargumenter i Haskell, må du importere modulet `System.Environment` og bruke funksjonen `getArgs`. Dette gjør det mulig å få tilgang til en liste med alle argumentene som er angitt i kommandolinjen når programmet kjøres.

La oss se på et eksempel:
```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Kommandolinje argumentene er: " ++ show args)
```
I dette eksempelet bruker vi `putStrLn` for å skrive ut teksten "Kommandolinje argumentene er:" etterfulgt av argumentene som blir angitt når programmet kjøres. `show` funksjonen brukes for å konvertere argumentlisten til en streng.

Når vi kjører dette programmet med følgende kommandolinje: `runhaskell arguments.hs argument1 argument2`, vil følgende output bli produsert:

```
Kommandolinje argumentene er: ["argument1","argument2"]
```
Som du kan se, returnerer `getArgs` funksjonen en liste med strenger som inneholder de angitte argumentene.

## Dypdykk

For å gjøre prosessen med å lese kommandolinjeargumenter enda mer fleksibel, kan du bruke funksjonen `System.Console.GetOpt` for å definere ulike alternativer og flagg som kan angis i kommandolinjen. Dette kan være spesielt nyttig hvis du har et komplisert program med flere konfigurasjonsalternativer.

Du kan også bruke `System.Environment.lookupEnv` funksjonen for å lese miljøvariabler som kan være nyttige for å konfigurere programmet ditt.

# Se også

- [System.Environment dokumentasjon](https://hackage.haskell.org/package/base/docs/System-Environment.html)
- [System.Console.GetOpt dokumentasjon](http://hackage.haskell.org/package/base-4.12.0.0/docs/System-Console-GetOpt.html)
- [Haskell Command Line Argument Parser](https://github.com/pcapriotti/optparse-applicative)