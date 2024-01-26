---
title:                "Trabajando con archivos CSV"
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?
El manejo de CSV (valores separados por comas) es básico e importante. Con él, lees y escribes datos en formato común usado para intercambiar información simple como listados y tablas. Programadores lo usan porque es un estándar muy compatible y fácil de manipular, especialmente útil en el análisis de datos y la transferencia entre diversas aplicaciones.

## Cómo hacerlo:
Para trabajar con CSV en Haskell, puedes usar el paquete `cassava`. Instálalo agregando `cassava` a tu archivo `.cabal` o ejecutando `cabal install cassava`. Aquí un ejemplo simple:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- Define tu estructura de datos
data Person = Person { name :: !String, age :: !Int }

-- Codifica una función para convertir tu dato a un registro CSV
instance ToNamedRecord Person where
    toNamedRecord (Person name age) = namedRecord ["name" .= name, "age" .= age]

-- Decodifica una función para leer un registro CSV y convertirlo a tu dato
instance FromNamedRecord Person where
    parseNamedRecord m = Person <$> m .: "name" <*> m .: "age"

main :: IO ()
main = do
  -- Leer CSV de un archivo
  csvData <- BL.readFile "people.csv"

  -- Decodificar CSV a una lista de `Person`
  case decodeByName csvData of
    Left err -> putStrLn err
    Right (_, vector) -> V.forM_ vector $ \person ->
      putStrLn $ name person ++ " tiene " ++ show (age person) ++ " años."
```

Si tienes un archivo `people.csv` así:
```
name,age
Alice,42
Bob,29
```

El output sería:
```
Alice tiene 42 años.
Bob tiene 29 años.
```

## Profundización:

- **Contexto histórico**: CSV se originó en la década de 1970 como un formato de archivo sencillo y ha sido ampliamente adoptado desde entonces.
- **Alternativas**: Además de `cassava`, puedes considerar `pandoc`, `csv-conduit` o simplemente usar `Data.List.Split` para casos muy básicos.
- **Detalles de implementación**: `cassava` aprovecha el sistema de tipos de Haskell y la clase de tipos de alto rendimiento para ofrecer tanto decodificación estricta como perezosa, además de un conjunto de funciones para trabajar con `Data.ByteString` que es eficiente para grandes archivos CSV.

## Ver También:

- Paquete `cassava`: [Hackage cassava](https://hackage.haskell.org/package/cassava)
- Tutorial sobre Haskell CSV: [Haskell CSV tutorial](https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/)
- Documentación de `ByteString`: [Hackage ByteString](https://hackage.haskell.org/package/bytestring)
