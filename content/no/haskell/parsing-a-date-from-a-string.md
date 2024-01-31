---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:36:32.407507-07:00
simple_title:         "Tolke en dato fra en streng"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av dato fra en streng betyr å omdanne tekst til et dato-objekt. Programmerere gjør dette for å enkelt manipulere og sammenligne datoer, og for å integrere brukerinput i programmer.

## Hvordan:
For å parse en dato i Haskell, bruk biblioteket `time` og funksjonen `parseTimeM`. Her er et eksempel:

```Haskell
import Data.Time

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

main :: IO ()
main = do
    let dateString = "2023-04-05"
    print $ parseDate dateString
```

Kjører du dette vil utdata være:

```Haskell
Just 2023-04-05
```

Hvis du gir en ugyldig dato, vil funksjonen returnere `Nothing`.

## Dypdykk
Historisk har datainnlesing og -parsing vært et vanlig problem. I Haskell lar `time` biblioteket brukere enkelt håndtere datoer og klokkeslett. Det finnes alternativer som `Data.Time.Calendar` og `Data.Time.Clock`, som tilbyr mer funksjonalitet for spesifikke situasjoner. Parsing av datoer innebærer å tolke strenger basert på forventet mønster – i vårt eksempel `%Y-%m-%d` for året, måneden og dagen.

## Se Også
For mer informasjon om `time` biblioteket, sjekk ut:

- Hackage `time` pakken: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time)
