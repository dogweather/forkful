---
title:                "Haskell: Leser kommandolinjeargumenter"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor bør noen interessere seg for å lese kommandolinje-argumenter? Det er viktig å kunne lese og behandle denne informasjonen for å kunne skrive effektive og fleksible programmer.

## Hvordan
Det finnes flere måter å lese kommandolinje-argumenter på i Haskell, men følgende metode vil være den enkleste og mest fleksible. Først må vi importere System.Environment og bruke getArgs funksjonen for å lagre de argumentene som programmet mottar fra kommandolinjen.

```Haskell
import System.Environment --importerer nødvendig modul
main = do
  args <- getArgs -- lagrer argumentene i en liste kalt args
  print args -- printer listen med argumenter
```

La oss si at vi kaller programmet vårt med følgende kommandolinje-argumenter:
```Haskell
./program arg1 arg2 arg3
```

Da vil utskriften bli:
```Haskell
["arg1", "arg2", "arg3"]
```

Som vi kan se, har getArgs-funksjonen lagret argumentene i en liste med strenger. Dette gjør det enkelt å arbeide med dem videre i koden.

## Dykk dypere
Det finnes også en annen måte å lese kommandolinje-argumenter på, ved hjelp av getProgName og getArgs-funksjonene. Denne metoden gir oss også mulighet til å arbeide med programmets navn i tillegg til argumentene.

```Haskell
import System.Environment
main = do
  progName <- getProgName
  args <- getArgs
  print ("Programnavn: " ++ progName)
  print ("Argumenter: " ++ show args)
```

Kjører vi programmet med samme argumenter som ovenfor, vil utskriften bli:
```Haskell
Programnavn: program
Argumenter: ["arg1", "arg2", "arg3"]
```

Vi kan også bruke funksjonen `lookupEnv` fra System.Posix.Env til å sjekke om et visst kommandolinje-argument er definert. På denne måten kan vi gjøre programmet mer robust og unngå unødvendige feil.

## Se også
- [Dokumentasjon for System.Environment-modulen](https://hackage.haskell.org/package/base/docs/System-Environment.html)
- [Tutorial om arbeid med kommandolinje-argumenter i Haskell](https://www.tutorialspoint.com/what-are-command-line-arguments-in-haskell)

Takk for at du leste denne bloggposten om lesing av kommandolinje-argumenter i Haskell. Vi håper du har lært noe nyttig og fått en bedre forståelse for dette konseptet. Lykke til med å implementere det i dine egne programmer!