---
title:    "Haskell: Lesing av kommandolinje-argumenter"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

# Hvorfor
Har du noen gang lurt på hvordan du kan gjøre programmet ditt mer fleksibelt ved å ta imot argumenter fra kommandolinjen? I denne blogginnlegget vil vi utforske hvordan man kan lese kommandolinjeargumenter i Haskell.

# Hvordan
Det første vi må gjøre er å importere System.Environment modulen i Haskell. Dette gir oss tilgang til funksjoner som lar oss lese kommandolinjeargumentene. Vi kan deretter bruke funksjonen `getArgs` for å få en liste med alle argumentene som ble gitt til programmet.

```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn $ "Antall argumenter: " ++ show (length args)
    mapM_ putStrLn args
```

I dette eksempelet bruker vi `mapM_` for å skrive ut hvert argument på en ny linje. `args` er en liste over strenger, så vi bruker `putStrLn` for å skrive ut hver streng. Med `length` funksjonen kan vi også telle antall argumenter som ble gitt til programmet.

For å kjøre programmet med noen argumenter, kan du åpne terminalen og skrive:

```bash
runhaskell <filnavn>.hs argument1 argument2 argument3
```

Dette vil skrive ut:

```
Antall argumenter: 3
argument1
argument2
argument3
```

# Dypdykk
Nå som vi har en grunnleggende forståelse for hvordan vi kan lese kommandolinjeargumenter, la oss se på noen mer detaljerte funksjoner.

Som vi så i eksempelet over, er `getArgs` en funksjon som gir oss en liste av argumenter som ble gitt til programmet. Men hva om vi kun er interessert i å lese det første argumentet? Da kan vi bruke funksjonen `getProgName` som vil gi oss navnet på programmet vårt, og deretter bruke `head` funksjonen for å få det første argumentet.

```Haskell
import System.Environment

main = do
    progName <- getProgName
    firstArg <- head <$> getArgs
    putStrLn $ "Programnavn: " ++ progName
    putStrLn $ "Første argument: " ++ firstArg
```

Dette vil skrive ut:

```
Programnavn: filnavn.hs
Første argument: argument1
```

En annen nyttig funksjon er `lookupEnv` som lar oss sjekke om et miljøvariabel er satt. Dette kan være nyttig hvis vi vil ha noe programatisk logikk basert på om en variabel er satt eller ikke. For eksempel kan vi ha en variabel `DEBUG_MODE` som vi kan sjekke for å bestemme om programmet skal skrive ut mer detaljert informasjon.

# Se også
- [Haskells dokumentasjon for System.Environment modulen](https://hackage.haskell.org/package/base-4.14.0.0/docs/System-Environment.html)
- [Bruk av kommandolinjeargumenter i et Haskell-prosjekt](https://www.fpcomplete.com/haskell/tutorial/args/)