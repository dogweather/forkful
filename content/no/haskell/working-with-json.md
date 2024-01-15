---
title:                "Å jobbe med json"
html_title:           "Haskell: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-json.md"
---

{{< edit_this_page >}}

# Hvorfor

Å jobbe med JSON i Haskell kan gi deg muligheten til å enkelt behandle og håndtere data i form av strukturerte objekter. Dette kan være nyttig for å kommunisere med andre programmer, eller for å organisere og analysere store datasett.

# Hvordan

For å jobbe med JSON i Haskell, kan du bruke biblioteket `aeson`. Følgende kode viser et eksempel på hvordan du parser en JSON-streng:

```Haskell
import Data.Aeson

data Person = Person
  { name :: String
  , age :: Int
  } deriving (Show, Generic) -- Deriving Generic gjør at biblioteket automatisk kan generere instanser av `ToJSON` og `FromJSON`

-- Definerer en instans av `FromJSON` for `Person`
instance FromJSON Person

-- Eksempel på en JSON-streng
jsonStr = "{\"name\": \"Per\", \"age\": 25}"

-- Bruker `parseMaybe` for å parse JSON-strengen og returnere en `Maybe`-verdi
result = parseMaybe (.: "name") <=< parseJSON $ jsonStr :: Maybe String
```

Koden over vil parse JSON-strengen og hente ut navnet `Per` som en `Maybe`-verdi. Med `aeson` kan du også enkelt lage og sende JSON-objekter ved å bruke funksjonen `encode`.

# Dypdykk

Biblioteket `aeson` gir en rekke ulike funksjoner for å behandle JSON-data. Du kan blant annet lage egendefinerte datatype og definere instanser av `ToJSON` og `FromJSON` for disse. Dette gjør det enkelt å tilpasse JSON-lesing og skriving etter dine behov. Det finnes også muligheter for å håndtere mer komplekse JSON-datastrukturer, som å inkludere lister, structs og arrays. Sjekk ut dokumentasjonen for `aeson` for å lære mer om alle mulighetene dette biblioteket har å tilby.

# Se også

- [aeson documentation](https://hackage.haskell.org/package/aeson)
- [JSON på Wikipedia](https://no.wikipedia.org/wiki/JSON)
- [En introduksjon til Haskell](https://wiki.haskell.org/Introduksjon)