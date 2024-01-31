---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON ist ein Datenformat, das für den Datenaustausch zwischen Server und Webanwendungen verwendet wird. Es ist weit verbreitet, weil es einfach, lesbar und sprachunabhängig ist.

## How to:
In Haskell benutzen wir das `aeson` Paket, um mit JSON zu arbeiten. Zuerst musst du es installieren:

```bash
cabal install aeson
```

Dann kannst du es in deinem Haskell-Code verwenden:

```Haskell
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import GHC.Generics

data User = User {
  name :: String,
  age  :: Int
} deriving (Show, Generic)

instance ToJSON User
instance FromJSON User

main :: IO ()
main = do
  let user = User "Max Mustermann" 30
  let json = encode user
  print json
  
  -- Output ist ein JSON String
  -- "{\"name\":\"Max Mustermann\",\"age\":30}"

  -- JSON String in ein User-Objekt umwandeln
  let decodedUser = decode "{\"name\":\"Max Mustermann\",\"age\":30}" :: Maybe User
  print decodedUser
  
  -- Output ist Just (User {name = "Max Mustermann", age = 30})
```

## Deep Dive
JSON steht für JavaScript Object Notation und wurde Anfang der 2000er Jahre populär. Als Alternative kann XML verwendet werden, allerdings ist JSON leichtgewichtiger und schneller zu verarbeiten. Technische Details: `aeson` verwendet Typable- und Generic-Features von GHC, um Haskell-Datentypen automatisch in JSON zu serialisieren und zu deserialisieren.

## See Also
- `aeson` Paket: https://hackage.haskell.org/package/aeson
- JSON Specifikation (RFC 7159): https://tools.ietf.org/html/rfc7159
- Tutorial zum Haskell/JSON-Handling: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
