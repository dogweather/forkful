---
title:    "Haskell: Konvertere en datoen til en streng"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Hvorfor

Konvertering av datoer til strenger er en vanlig oppgave for utviklere, spesielt når man jobber med dataprogrammering og databasesystemer. Å ha en god forståelse av hvordan man håndterer datoer og formaterer dem som strenger er derfor en viktig ferdighet å ha for enhver Haskell-utvikler.

# Hvordan

For å konvertere en dato til en streng i Haskell, bruker vi funksjonen "formatTime" fra "Data.Time" biblioteket. La oss si at vi vil konvertere en dato til strengen "01.01.2020". Først må vi importere Data.Time biblioteket og definere datoen som en verdi av typen "Day". Deretter bruker vi "formatTime" funksjonen og spesifiserer ønsket datoformat som en streng. Her er et eksempel på kode og et eksempel på output:

```Haskell
import Data.Time

dato = fromGregorian 2020 1 1
formatertDato = formatTime defaultTimeLocale "%d.%m.%Y" dato

print formatertDato
--output: "01.01.2020"
```

Vi kan også bruke andre formateringsalternativer, for eksempel "%b %d, %Y" for å få et format som "Jan 01, 2020". Det er viktig å merke seg at det finnes flere formateringsalternativer og du kan også lage dine egne tilpassede formater.

# Dyp dykk

Haskell har en strengt typet tilnærming til programmering, og det samme gjelder for datoer og strenger. Når vi konverterer en dato til en streng, må vi sørge for at vi spesifiserer riktig datoformat som passer til datotypen vår. Vi kan for eksempel ikke bruke "Mai" som månedsnavn hvis datoen vår er definert som en numerisk verdi.

En annen ting å være oppmerksom på er hvordan Haskell håndterer tidssoner og sommertid. Det er viktig å forstå disse konseptene når man jobber med datoer og tid i Haskell, da det kan påvirke resultatet av konverteringen. Det kan også være lurt å lese dokumentasjonen til "Data.Time" biblioteket for å få en dypere forståelse av hvordan funksjonene fungerer.

# Se også

- Offisiell dokumentasjon for [Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- En guide til [dato og tid i Haskell](https://en.wikibooks.org/wiki/Haskell/Programming_in_Haskell/Dates_and_times)
- Et [diskusjonsforum](https://stackoverflow.com/questions/1133656/formatting-difficulties-with-datetype-in-haskell) om konvertering av datoer i Haskell