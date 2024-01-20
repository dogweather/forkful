---
title:                "Praca z JSON"
html_title:           "Bash: Praca z JSON"
simple_title:         "Praca z JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
JSON, czyli JavaScript Object Notation, to standardowy format wymiany danych używany głównie w sieci. Programiści używają go ze względu na prostotę, czytelność i uniwersalność – można nim reprezentować większość danych potrzebnych w aplikacjach webowych.

## Jak to zrobić:
W Haskellu używamy pakietu `aeson` do pracy z JSON. Instalujemy go przez `cabal install aeson`. Poniżej przekształcamy Haskellowy typ na JSON i odwrotnie.

```Haskell
{-# LANGUAGE DeriveGeneric #-}
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as B

-- Definiujemy typ danych
data Osoba = Osoba
  { imie :: String
  , wiek :: Int
  } deriving (Show, Generic)

-- Automatyczne pochodzenie instancji FromJSON i ToJSON dzięki GHC.Generics
instance FromJSON Osoba
instance ToJSON Osoba

main :: IO ()
main = do
  -- Serializacja do JSON
  let osoba = Osoba "Jan" 30
  let jsonOsoba = encode osoba
  B.putStrLn jsonOsoba

  -- Deserializacja z JSON
  let maybeOsoba = decode jsonOsoba :: Maybe Osoba
  case maybeOsoba of
    Just o -> print o
    Nothing -> putStrLn "Błąd przy deserializacji!"
```

Sample output:
```
{"wiek":30,"imie":"Jan"}
Osoba {imie = "Jan", wiek = 30}
```

## Deep Dive
JSON powstał w 2001 roku jako alternatywa dla XML. W Haskellu, `aeson` jest standardem, ale istnieją też inne biblioteki jak `json` czy `jsonb`. Wszystko kręci się wokół enkodowania i dekodowania danych, co `aeson` robi wykorzystując `FromJSON` i `ToJSON`. Używa on `GHC.Generics` do automatycznego generowania niezbędnych instancji, co znacząco ułatwia pracę.

## Zobacz też:
- Oficjalna dokumentacja `aeson` na Hackage: [hackage.haskell.org/package/aeson](https://hackage.haskell.org/package/aeson)
- Specyfikacja JSON: [www.json.org/json-pl.html](http://www.json.org/json-pl.html)