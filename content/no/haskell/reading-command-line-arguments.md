---
title:                "Lesing av kommandolinjeargumenter"
html_title:           "Haskell: Lesing av kommandolinjeargumenter"
simple_title:         "Lesing av kommandolinjeargumenter"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Av og til kan vi ønske at programmet vårt kan utføre forskjellige handlinger basert på informasjon som er gitt når programmet kjører. Et eksempel på dette kan være å la brukeren angi ulike filnavn, parametere eller andre innstillinger via kommandolinjen. For å lære hvordan man kan gjøre dette i Haskell, les videre.

# Hvordan

Det første vi må gjøre er å importere funksjonen `getArgs` fra modulet `System.Environment`. Denne funksjonen tar ingen parametere, men gir tilbake en liste med strenger som representerer kommandolinje argumentene som er gitt når programmet kjøres.

```Haskell 
import System.Environment

main = do
    args <- getArgs
    putStrLn (head args)
```
Dette enkle programmet vil få tak i det første kommandolinje argumentet og skrive det ut til konsollen.

Eksempel på kjøring: 
```
$ runhaskell args.hs argument1
argument1
```

Det er også mulig å behandle flere argumenter ved hjelp av funksjoner som `map` eller `foldr`.

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn (unwords (map show args))
```

I dette tilfellet vil alle argumentene skrives ut, konvertert til strenger.

Eksempel på kjøring: 
```
$ runhaskell args.hs argument1 argument2 argument3
"argument1 argument2 argument3"
```

# Dykk dypere

Kommandolinje argumenter er lett å hente ut, men det finnes også noen ting å være oppmerksom på. For det første vil programmet alltid motta minst ett argument - nemlig navnet på programmet som kjører. For det andre vil alle argumentene være av typen `String`, noe som betyr at eventuelle tall eller andre datatyper må konverteres til riktig type før de kan brukes i programmet.

Et eksempel på dette kan være å lese inn et tall som et kommandolinje argument og deretter behandle det som et tall i koden:

```Haskell
import System.Environment

main = do
    args <- getArgs
    let num = read (head args) :: Int
    print (num * 2)
```

Eksempel på kjøring: 
```
$ runhaskell args.hs 5
10
```

Det finnes også ulike biblioteker som kan hjelpe deg med å behandle kommandolinje argumenter på en enklere måte, som for eksempel `optparse-applicative` eller `cmdargs`.

# Se også

- [Haskell dokumentasjon - System.Environment](https://www.haskell.org/cabal/users-guide/developing-packages.html)
- [Haskell dokumentasjon - optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
- [Haskell dokumentasjon - cmdargs](https://hackage.haskell.org/package/cmdargs)