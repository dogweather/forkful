---
title:                "Haskell: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué trabajar con CSV en Haskell

CSV (Comma Separated Values) son archivos de texto que contienen datos separados por comas. Son una forma muy común de almacenar y trabajar con datos en la programación. En Haskell, trabajar con CSV puede ser muy beneficioso ya que nos permite manipular datos de manera sencilla y eficiente.

## Cómo hacerlo

Para trabajar con CSV en Haskell, podemos usar la biblioteca "cassava". Esta biblioteca nos permite leer y escribir archivos CSV utilizando funciones específicas.

Primero, debemos importar la biblioteca en nuestro código:

```Haskell
import Data.Csv
```

En caso de que estemos trabajando con un archivo CSV, podemos leerlo utilizando la función "decodeFile":

```Haskell
records <- decodeFile "datos.csv" :: IO (Either String (Vector Vector String))
```

La función "decodeFile" devuelve una estructura de datos llamada "Vector", que contiene los datos del archivo CSV. Podemos acceder a los datos de esta manera:

```Haskell
case records of
    (Left err) -> do putStrLn err
    (Right row) -> do
        let firstRow = row ! 0
        let firstValue = firstRow ! 0
        print $ "El primer valor es: " ++ firstValue
```

Para escribir datos en un archivo CSV, podemos utilizar la función "encodeFile":

```Haskell
encodeFile "nuevos_datos.csv" [
    ["Nombre", "Edad", "Género"],
    ["Ana", "22", "Femenino"],
    ["Juan", "28", "Masculino"]
    ]
```

Esta función tomará una lista de listas, donde cada lista interna representa una fila en el archivo CSV.

## Profundizando

La biblioteca "cassava" también nos permite trabajar con tipos de datos personalizados. Podemos definir un tipo de datos para representar una fila en nuestro archivo CSV:

```Haskell
data Persona = Persona {
    nombre :: String,
    edad :: Int,
    genero :: String
} deriving (Generic, Show)

instance ToNamedRecord Persona
instance FromNamedRecord Persona
```

Luego, podemos leer y escribir nuestro archivo CSV utilizando este tipo de datos:

```Haskell
records <- decodeFile "personas.csv" :: IO (Either String (Vector Persona))

encodeFile "nuevas_personas.csv" (fromVector (Vector.fromList [
    Persona "Ana" 22 "Femenino",
    Persona "Juan" 28 "Masculino"
    ]))
```

Podemos ver que trabajar con CSV en Haskell es bastante sencillo y nos permite manipular datos de manera flexible y eficiente.

## Ver también

- Documentación de la biblioteca "cassava": https://hackage.haskell.org/package/cassava
- Tutoriales de Haskell: https://www.haskell.org/documentation/