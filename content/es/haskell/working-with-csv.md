---
title:                "Trabajando con csv"
html_title:           "Haskell: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con CSV en Haskell se refiere a manipular archivos de texto separados por comas, que son comúnmente utilizados para almacenar datos en forma de tablas. Los programadores a menudo necesitan trabajar con CSV para leer y escribir datos que provienen de diferentes fuentes o para convertir datos en diferentes formatos.

## ¿Cómo hacerlo?

Para leer un archivo CSV en Haskell, se puede utilizar la función `readCSV` de la librería `Data.CSV`. Por ejemplo:

```Haskell
import Text.CSV

main :: IO ()
main = do
  csvData <- parseCSVFromFile "datos.csv" -- lee el archivo CSV
  case csvData of
    Left err -> putStrLn $ "Error al leer el archivo: " ++ show err -- maneja posibles errores
    Right records -> print records -- imprime los registros en una lista de listas
```

El archivo CSV `datos.csv` podría contener algo como:

```
Nombre,Edad,Ciudad
Juan,25,Madrid
María,30,Barcelona
Pablo,28,Valencia
```

La salida sería:

```
[["Nombre", "Edad", "Ciudad"], ["Juan", "25", "Madrid"], ["María", "30", "Barcelona"], ["Pablo", "28", "Valencia"]]
```

Para escribir en un archivo CSV, se puede utilizar la función `writeCSV` de la misma librería. Por ejemplo:

```Haskell
import Text.CSV

main :: IO ()
main = do
  let dataToWrite = [["Nombre", "Edad", "Ciudad"], ["Juan", "25", "Madrid"], ["María", "30", "Barcelona"], ["Pablo", "28", "Valencia"]]
  let csvData = unlines $ map (printCSVRecord ", ") dataToWrite -- convierte la lista de listas en una cadena con los datos separados por comas
  writeFile "nuevos_datos.csv" csvData -- escribe en el archivo
```

El archivo CSV `nuevos_datos.csv` contendría lo mismo que la lista de listas `dataToWrite`.

## Dive Profundo

El formato de archivo CSV fue creado en la década de 1970 para facilitar el intercambio de datos entre diferentes programas. Aunque su simplicidad lo hace popular en la actualidad, no es adecuado para datos complejos o sensibles debido a su falta de estructura y de tipos de datos. Alternativas para trabajar con datos estructurados en Haskell incluyen el uso de bases de datos y de librerías como `cassava` y `opencsv`. La implementación de la librería `Data.CSV` está basada en el estándar RFC 4180.

## Ver También

- [Documentación de la librería Data.CSV](https://hackage.haskell.org/package/csv)
- [RFC 4180 sobre formato CSV](https://tools.ietf.org/html/rfc4180)
- [Librería cassava para trabajar con CSV en Haskell](https://hackage.haskell.org/package/cassava)
- [Librería opencsv para trabajar con CSV en Haskell](https://hackage.haskell.org/package/opencsv)