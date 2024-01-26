---
title:                "Arbeta med JSON"
html_title:           "Arduino: Arbeta med JSON"
simple_title:         "Arbeta med JSON"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Jobbar med JSON innebär att hantera data i JavaScript Object Notation - ett lättviktigt datautbytesformat. Programmerare gör detta för att enkelt skicka och ta emot data över nätet samt att lagra och arbeta med data i en struktuerad form.

## Hur man gör:
Haskell hanterar JSON med bibliotek som `aeson`. Här är ett enkelt exempel:

```haskell
import Data.Aeson
import Data.ByteString.Lazy as B
import Data.Text
import Control.Monad (mzero)

-- För att representera en enkel JSON-struktur skapar vi en data-typ.
data User = User
  { userId :: Int
  , userName :: Text
  } deriving Show

-- Lägger till instanser för att göra det till JSON:
instance FromJSON User where
  parseJSON (Object v) = User <$>
                         v .: "userId" <*>
                         v .: "userName"
  parseJSON _          = mzero

instance ToJSON User where
  toJSON (User userId userName) =
    object ["userId" .= userId, "userName" .= userName]

-- Exempel för att tolka (parse) JSON:
main :: IO ()
main = do
  -- Anta att vi har JSON-data som en ByteString
  let jsonBytes = "{\"userId\": 123, \"userName\": \"Ada\"}" :: ByteString
  let maybeUser = decode jsonBytes :: Maybe User
  case maybeUser of
    Just user -> print user
    Nothing -> putStrLn "Couldn't decode JSON"
```

Exempel output:

```
User {userId = 123, userName = "Ada"}
```

## Djupdykning
JSON introducerades 2001, motiverat av JavaScripts växande popularitet för webbutveckling. Alternativ till JSON inkluderar XML och YAML. `aeson`, ett Haskell-paket, är optimerat för prestanda och minnesanvändning. Dess användning av `ByteString` minimerar overhead vid in-/utläsning av JSON-data.

## Se även
- The `aeson` hemsida: http://hackage.haskell.org/package/aeson
- Mer om JSON: https://www.json.org/json-sv.html
- Om `ByteString`: https://hackage.haskell.org/package/bytestring
