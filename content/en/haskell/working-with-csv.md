---
title:                "Working with csv"
html_title:           "C recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Working with CSV (Comma-Separated Values) means reading and writing data in a tabular form. Programmers use CSV due to its simplicity and widespread support across systems for data exchange.

## How to:
To handle CSV in Haskell, you can use the `cassava` library. Install it by adding `cassava` to your `.cabal` file or using Stack. Here's how to decode and encode CSV data:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Assume we're working with this type
type Person = (String, Int, Bool)

-- Sample CSV data
csvData :: BL.ByteString
csvData = "John Doe,30,true\nJane Smith,25,false"

-- Decoding CSV data
decodePeople :: BL.ByteString -> Either String (V.Vector Person)
decodePeople = fmap snd . decode NoHeader

-- Encoding data to CSV
encodePeople :: V.Vector Person -> BL.ByteString
encodePeople = encode

-- Usage example
main :: IO ()
main = do
  -- Decoding
  case decodePeople csvData of
    Left err -> putStrLn err
    Right v -> print v
  
  -- Encoding
  let people = V.fromList [("Alice", 23, True), ("Bob", 35, False)]
  BL.putStrLn $ encodePeople people
```

Sample output:
```plaintext
[("John Doe",30,True),("Jane Smith",25,False)]
"Alice",23,True
"Bob",35,False
```

## Deep Dive
CSV handling in Haskell has evolved. Earliest methods involved manual string parsing, which was error-prone. `cassava` provides type-safe parsing, leaning on Haskell's strong type system. Alternatives include the `csv` package, but `cassava` is more efficient and flexible. Implementation-wise, `cassava` uses streams for memory efficiency and speed, which is important when dealing with large datasets.

## See Also
- The `cassava` library on Hackage: https://hackage.haskell.org/package/cassava
- Haskell's ByteString library for handling binary data: https://hackage.haskell.org/package/bytestring
- A guide to the Vector library, for efficient lists: https://hackage.haskell.org/package/vector