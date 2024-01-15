---
title:                "Arbeid med yaml"
html_title:           "Haskell: Arbeid med yaml"
simple_title:         "Arbeid med yaml"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du skal jobbe med data eller konfigurasjonsfiler i Haskell, vil du sannsynligvis støte på YAML-formatet. YAML (YAML Ain’t Markup Language) er et populært format for å representere datastrukturer, og er spesielt godt egnet for å lagre konfigurasjonsinnstillinger.

## Hvordan

For å kunne jobbe med YAML-filer i Haskell, må du først importere noen moduler. Du trenger modulene `System.IO`, `Data.Yaml` og `Control.Exception` for å kunne lese og skrive YAML-filer. Her er et eksempel på hvordan du kan lese en YAML-fil og konvertere den til en Haskell-datastruktur:

```Haskell
import System.IO (withFile, IOMode (ReadMode))
import Data.Yaml (decodeFileThrow)
import Control.Exception (try)

data Person = Person
    { name :: String
    , age :: Int
    , occupation :: String
    } deriving (Show, Generic)

instance FromJSON Person
```

Funksjonen `withFile` åpner filen for lesing, og funksjonen `decodeFileThrow` dekoder den til en Haskell-datastruktur. `try`-blokken tar hånd om eventuelle feil som kan oppstå under lesing. Som en bonus er det også brukt `Generic` for å automatisk generere en `FromJSON`-instance for `Person`.

## Dypdykk

Hvis du vil lære mer om YAML og hvordan det fungerer, kan du sjekke ut YAML-spesifikasjonen (https://yaml.org/spec/) som kan gi deg en bedre forståelse av formatet. Det er også mulig å bruke YAML-biblioteker som kan hjelpe deg med å generere og validere YAML-kode.

## Se også

- Offisiell YAML-spesifikasjon: https://yaml.org/spec/
- Haskell-pakker for å jobbe med YAML: https://hackage.haskell.org/packages/search?terms=yaml