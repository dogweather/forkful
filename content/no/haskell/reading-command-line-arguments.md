---
title:                "Lesing av kommandolinje-argumenter"
html_title:           "Haskell: Lesing av kommandolinje-argumenter"
simple_title:         "Lesing av kommandolinje-argumenter"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Lesing av kommandolinjeargumenter er en vanlig og nyttig praksis for Haskell-programmere. Dette innebærer å lese inn informasjon som brukes til å konfigurere og kjøre et program gjennom kommandolinjen. Dette gjør det mulig for brukere å angi spesifikke parametere og verdier til programmet, noe som gir større fleksibilitet og kontroll.

## Hvordan:
Vi kan lese kommandolinjeargumenter i Haskell ved å bruke funksjonen `getArgs` fra modulen `System.Environment`. Dette returnerer en liste av strenger som representerer hvert argument som ble gitt når programmet ble kjørt. Vi kan deretter behandle og bruke disse argumentene i vårt program.

```Haskell
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("Antall argumenter: " ++ show (length args))
  putStrLn ("Argumenter: " ++ show args)
```

Når vi kjører dette programmet med kommandoen `runhaskell read_args.hs arg1 arg2 arg3`, vil output være:
```
Antall argumenter: 3
Argumenter: ["arg1","arg2","arg3"]
```

## Dypdykk:
Selv om det kan virke som en enkel funksjon, har `getArgs` en viktig rolle i Haskell-programmering. I tidligere versjoner av Haskell ble kommandolinjeargumenter lest inn ved hjelp av funksjonen `getArgs` fra modulen `System`. Denne versjonen behandlet argumentene som strenger, mens den nåværende versjonen behandler dem som uendelige lister av strenger.

Det finnes også alternative måter å lese kommandolinjeargumenter på i Haskell, for eksempel ved å bruke biblioteket `optparse-applicative`. Dette biblioteket gir et mer robust og elegant grensesnitt for å håndtere argumenter.

Det er også verdt å merke seg at kommandolinjeargumenter ikke er den eneste måten å gi input til et Haskell-program. Det er også mulig å lese inn filer eller data direkte fra terminalen.

## Se også:
- [Haskell dokumentasjon for System.Environment](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html)
- [Optparse-applicative biblioteket](https://hackage.haskell.org/package/optparse-applicative)