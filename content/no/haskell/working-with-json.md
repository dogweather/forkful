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

## Hva og hvorfor?
Arbeidet med JSON handler om å håndtere data i et format som er lett for datamaskiner å tolke og for mennesker å lese. Dette er viktig for å kunne utveksle og manipulere data på en effektiv måte. Programmere bruker JSON for å lagre og transportere data mellom forskjellige programmer og systemer.

## Slik gjør du det:
Haskell har innebygd støtte for å håndtere JSON gjennom et bibliotek kalt "aeson". Du kan starte ved å importere biblioteket og definere typene du ønsker å arbeide med.

```Haskell
import Data.Aeson

data Person = Person { name :: String, age :: Int } deriving (Show)

instance FromJSON Person where 
    parseJSON (Object v) = Person <$> v .: "name" <*> v .: "age"
```

Deretter kan du bruke funksjonen `decode` for å konvertere en JSON-streng til en Haskell-verdi.

```Haskell
main = do
    let jsonStr = "{\"name\":\"Bob\", \"age\": 25}"
    case decode jsonStr :: Maybe Person of
        Just person -> putStrLn $ "Personens navn er " ++ name person ++ " og han er " ++ show (age person) ++ " år gammel."
        Nothing -> putStrLn "Klarte ikke å konvertere JSON til en Person-verdi."
```

Output:
```
Personens navn er Bob og han er 25 år gammel.
```

## Dypdykk:
JSON står for JavaScript Object Notation og ble utviklet som et enklere og mer lettvektig alternativ til XML. Det finnes mange alternative biblioteker for å arbeide med JSON i Haskell, for eksempel `JSON`, `HJSON` og `JSONStream`. I tillegg kan du også bruke enkodere og dekodere for JSON gjennom Haskell sine generiske programmeringsegenskaper.

## Se også:
- [Haskell Aeson dokumentasjon](https://hackage.haskell.org/package/aeson)
- [Alternativer for å arbeide med JSON i Haskell](https://wiki.haskell.org/JSON)