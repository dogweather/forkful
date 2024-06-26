---
date: 2024-02-03 19:03:03.666619-07:00
description: "How to: In Haskell, handling CSV files can be achieved using the `cassava`\
  \ library, one of the popular third-party libraries for this purpose. Below are\u2026"
lastmod: '2024-03-13T22:45:00.148510-06:00'
model: gpt-4-0125-preview
summary: In Haskell, handling CSV files can be achieved using the `cassava` library,
  one of the popular third-party libraries for this purpose.
title: Working with CSV
weight: 37
---

## How to:
In Haskell, handling CSV files can be achieved using the `cassava` library, one of the popular third-party libraries for this purpose. Below are examples showcasing how to read from and write to CSV files using `cassava`.

**1. Reading a CSV file:**

First, ensure you have `cassava` installed by adding it to your project's cabal file or using Stack.

Here's a simple example to read a CSV file and print each record. We assume the CSV file has two columns: name and age.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " is " ++ show (age :: Int) ++ " years old."
```

Assuming `people.csv` contains:
```
John,30
Jane,25
```
The output will be:
```
John is 30 years old.
Jane is 25 years old.
```

**2. Writing a CSV file:**

To create a CSV file, you can use the `encode` function from `cassava`.

Here’s how you could write a list of records to a CSV file:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

After running this program, `output.csv` will contain:

```
John,30
Jane,25
```

This concise introduction to working with CSV files in Haskell using the `cassava` library demonstrates how to both read from and write to CSV files, making data manipulation tasks more approachable for those new to the language.
