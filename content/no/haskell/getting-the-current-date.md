---
title:                "Få dagens dato"
html_title:           "Haskell: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hva & hvorfor?
Å få opp datoen i Haskell er en måte å få programmet ditt til å vise den aktuelle datoen på. Dette er nyttig for å holde styr på når et program ble kjørt eller når en spesiell funksjon ble utført. Det er også en viktig del av å skrive pålitelige og nøyaktige programmer.

# Hvordan:
For å få den nåværende datoen i Haskell, kan du bruke funksjonen "getCurrentTime" fra "Data.Time" biblioteket. Funksjonen returnerer en datotype "UTCTime" som inneholder informasjon om den aktuelle datoen og klokkeslettet. Du kan deretter bruke andre funksjoner i "Data.Time" biblioteket for å få den ønskede formaten på datoen.

```Haskell
import Data.Time

main = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    putStrLn $ "Dagens dato er: " ++ show (day, month, year)

-- Output: Dagens dato er: (27,6,2020)
```

# Dypdykk:
Funksjonen "getCurrentTime" ble introdusert i Haskell i versjon 1.4 av "time" biblioteket. Før dette måtte utviklere bruke funksjoner som "formatCalendarTime" for å få den aktuelle datoen. Det finnes også alternativer som "System.Clock" som gir mer nøyaktig informasjon om tiden.

# Se også:
[Data.Time biblioteket](https://www.haskell.org/hoogle/?hoogle=Data.Time)
[Haskell dokumentasjon for "time" biblioteket](https://hackage.haskell.org/package/time)
[System.Clock dokumentasjon](https://hackage.haskell.org/package/time/docs/System-Clock.html)