---
title:                "Haskell: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

Når du jobber med programmering, kan det ofte være nødvendig å sammenligne to datoer. Dette kan være nyttig når du for eksempel skal sortere data eller utføre beregninger basert på datoer. I denne bloggposten vil vi se på hvordan du kan sammenligne to datoer i Haskell.

# Hvordan

For å sammenligne to datoer i Haskell, må du først importere modulen ```Data.Time```. Deretter kan du bruke funksjonen ```diffDays``` for å finne antall dager mellom to datoer. La oss se på et eksempel:

```Haskell
import Data.Time

startDate = fromGregorian 2020 1 1
endDate = fromGregorian 2020 8 31

diffDays startDate endDate
```

Dette vil returnere verdien 243 (dager), som er antall dager mellom startdatoen og sluttdatoen.

I tillegg til å finne antall dager, kan du også bruke funksjonene ```diffMinutes```, ```diffHours``` og ```diffMonths``` for å finne forskjellen mellom to datoer i henholdsvis minutter, timer og måneder.

For å sammenligne om en dato kommer før eller etter en annen dato, kan du bruke funksjonen ```compare```. Denne funksjonen vil returnere en ordre (LT, EQ eller GT) basert på hvilken dato som kommer først.

```Haskell
compare startDate endDate
```

Dette vil returnere verdien LT (Less Than), som betyr at startdatoen kommer før sluttdatoen. 

# Dypdykk

I Haskell er datatypen for datoer ```Day```, som representerer en spesifikk dato. Det er også mulig å bruke datatypen ```UTCTime```, som representerer en spesifikk dato og tid. I tillegg til de nevnte funksjonene, finnes det også mange andre funksjoner og operasjoner for å jobbe med datoer i Haskell.

# Se også

- [Data.Time dokumentasjon] (https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell Wiki-side om datoer] (https://wiki.haskell.org/Date_and_time)
- [Tutorial on comparing dates in Haskell] (https://www.schoolofhaskell.com/school/advanced-haskell/date-and-time)