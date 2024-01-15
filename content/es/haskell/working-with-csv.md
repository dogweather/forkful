---
title:                "Trabajando con archivos csv"
html_title:           "Haskell: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué utilizar CSV en Haskell?

Si estás buscando una forma rápida y sencilla de trabajar con grandes cantidades de datos tabulares, entonces el formato CSV es tu mejor amigo. Con Haskell, podrás procesar y manipular fácilmente estos datos, lo que lo convierte en una herramienta útil para proyectos de análisis de datos o administración de bases de datos.

## Cómo hacerlo

Para trabajar con CSV en Haskell, primero debes importar el módulo `Data.Csv` y la función `decodeDefault` que nos ayudará a convertir nuestro archivo CSV en una estructura de datos legible para Haskell. A continuación, debemos especificar la estructura de nuestro archivo CSV, utilizando tipos de datos personalizados si es necesario. Por ejemplo:

```Haskell
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Csv

data Employee = Employee
  { name :: String
  , age :: Int
  , department :: String
  } deriving Show

instance FromNamedRecord Employee where
  parseNamedRecord r = Employee <$> r .: "Nombre" <*> r .: "Edad" <*> r .: "Departamento"

main = do
  csvData <- BL.readFile "empleados.csv"
  case decodeDefault NoHeader csvData of
    Left err -> putStrLn err
    Right v -> mapM_ print (v :: [Employee])
```

En este ejemplo, definimos un tipo de datos `Employee` con campos `nombre`, `edad` y `departamento`. Luego, definimos una instancia de `FromNamedRecord` que especifica cómo se leerán los datos del archivo CSV. Finalmente, utilizamos la función `decodeDefault` para convertir el archivo a una lista de `Employee` y imprimir los resultados.

## Inmersión profunda

Si quieres trabajar con archivos CSV que no tengan encabezados, puedes usar `decode NoHeader` en lugar de `decodeDefault NoHeader`. Además, el módulo `Data.Csv` también ofrece funciones útiles para escribir datos en formato CSV, como `encodeDefaultOrderedByName` para ordenar los datos antes de escribirlos en el archivo.

## Véase también

- [Documentación oficial de Haskell](https://www.haskell.org/documentation/)
- [Tutorial de Haskell](https://www.haskell.org/tutorial/)