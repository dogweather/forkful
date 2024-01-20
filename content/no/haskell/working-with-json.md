---
title:                "Arbeid med JSON"
html_title:           "Arduino: Arbeid med JSON"
simple_title:         "Arbeid med JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Jobbing med JSON handler om å parse og generere data i JavaScript Object Notation-formatet, et lettvekts datautvekslingsformat. Programmerere gjør det fordi det er enkelt å lese for mennesker og lett å parse for maskiner.

## Hvordan:
For å jobbe med JSON i Haskell, bruker du ofte biblioteket `aeson`. Her er et raskt eksempel:

```haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B

data Person = Person
  { navn :: String
  , alder :: Int
  } deriving (Show, Generic)

instance ToJSON Person
instance FromJSON Person

main :: IO ()
main = do
  let person = Person "Ola" 25
  B.writeFile "person.json" (encode person)
  
  innlestPerson <- B.readFile "person.json"
  let decodedPerson = decode innlestPerson :: Maybe Person
  print decodedPerson
```

Kjører du dette, vil filen `person.json` inneholde JSON-representasjonen av `Person`, og programmet vil skrive ut: `Just (Person {navn = "Ola", alder = 25})`.

## Dypdykk:
JSON ble popularisert på 2000-tallet som et alternativ til XML. Haskell's `aeson`-biblioteket, inspirert av JavaScripts JSON-funksjoner, er nå standard for å håndtere JSON. Det bruker `DeriveGeneric` for automatisk instansgenerering, og `ByteString` for effektivitet. Alternativer inkluderer `json`- og `jsonb`-bibliotekene, men `aeson` anbefales for dets ytelse og fleksibilitet.

## Se Også:
- `aeson` biblioteket på Hackage: https://hackage.haskell.org/package/aeson
- Introduksjon til JSON: https://www.json.org/json-no.html
- En mer grundig guide til `aeson`: https://artyom.me/aeson