---
title:                "Haskell: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor bry seg med å lese kommandolinjeargumenter? Det kan være nyttig når du ønsker å gi programmene dine forskjellige input uten å endre koden hver gang.

## Slik gjør du det

Å lese kommandolinjeargumenter i Haskell er enkelt. Først må du importere System.Environment-modulen. Deretter kan du bruke funksjonen getArgs for å hente argumentene som ble gitt til programmet ditt. Her er et eksempel:

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Første argument: " ++ head args)
    putStrLn ("Andre argument: " ++ args !! 1)
```

`getArgs` returnerer en liste av strenger, der hvert element i listen er et argument gitt fra kommandolinjen. I eksempelet over bruker vi `head` og `!!`-operatorene for å få tak i de første to argumentene og skrive dem ut til konsollen.

Så når du kjører dette programmet fra kommandolinjen, for eksempel med argumentene "Hei" og "Haskell", vil outputen være:

```
Første argument: Hei
Andre argument: Haskell
```

Du kan også bruke funksjonen `length` for å få totalt antall argumenter gitt til programmet ditt, og deretter bruke en løkke for å få tak i alle argumentene.

## Dykk dypere

Nå som du vet hvordan du kan lese kommandolinjeargumenter, kan du begynne å bruke dette i mer komplekse programmer. For eksempel kan du ta inn brukerinput og bruke det som argumenter til funksjoner eller algoritmer.

Det er også viktig å være oppmerksom på at rekkefølgen på argumentene i `getArgs`-listen vil være den samme som rekkefølgen de ble gitt fra kommandolinjen. Dette kan være viktig hvis du skal lese inn forskjellige typer data, som for eksempel tall eller tekst.

## Se også

- [Haskell.org sin guide til kommandolinjeargumenter](https://www.haskell.org/onlinereport/haskell2010/haskellch11.html)
- [Haskell Wikibooks sin guide til system IO](https://en.wikibooks.org/wiki/Haskell/Input_and_output)