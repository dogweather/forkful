---
title:                "Trabajando con csv"
html_title:           "Elm: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con CSV?

Trabajar con archivos CSV puede ser muy útil en programas que necesitan manejar grandes cantidades de datos. Al utilizar Elm para trabajar con CSV, se pueden realizar tareas como importar y exportar datos de manera eficiente.

## Cómo hacerlo

Elm tiene una librería llamada `elm-csv` que facilita el manejo de archivos CSV. Para utilizarla, primero debemos instalarla mediante el comando `elm install elm/csv` en la terminal.

Una vez instalada, podemos utilizar la función `Decode.csv` para decodificar un archivo CSV en una estructura de datos en formato `List`.

```elm
import Csv.Decode as Decode

csvDecoder : Decode.Decoder (List (List Data))
csvDecoder =
    Decode.csv Decode.int

DecodedData : Result Errors (List (List Int))
DecodedData =
    Decode.decodeString csvDecoder "1,2,3\n4,5,6"

-- El resultado será: Ok [[1,2,3], [4,5,6]]
```

Un ejemplo más completo puede ser la decodificación de un archivo CSV con encabezados para crear una estructura de datos más compleja, como un registro de estudiantes con sus nombres, edades y carreras.

```elm
import Csv.Decode as Decode

type alias Student =
    { name : String
    , age : Int
    , major : String
    }

csvDecoder : Decode.Decoder (List Student)
csvDecoder =
    Decode.list <| Decode.indexedMap Student
        [ ( 0, Decode.Header "name" Decode.string )
        , ( 1, Decode.Header "age" Decode.int )
        , ( 2, Decode.Header "major" Decode.string )
        ]

studentData : Result Errors (List Student)
studentData =
    Decode.decodeString csvDecoder "name,age,major\nJohn,22,Computer Science\nSara,20,Mathematics"

-- El resultado será: Ok [{ name = "John", age = 22, major = "Computer Science" },
--                       { name = "Sara", age = 20, major = "Mathematics" }]
```

## Inmersión profunda

La librería `elm-csv` también nos permite trabajar con archivos CSV de manera bidireccional, es decir, tanto importar como exportar datos. Además, tiene funciones para manejar delimitadores, saltos de línea y manejo de errores.

También podemos utilizar la función `Decode.list` para decodificar un archivo CSV con múltiples filas y columnas en una lista de listas de datos, lo que nos da mayor flexibilidad en el manejo de los datos.

En resumen, trabajar con CSV en Elm no solo es fácil y eficiente, sino que también nos permite manipular grandes volúmenes de datos de manera sencilla. ¡Así que no dudes en utilizar esta librería en tus futuros proyectos!

## Ver también

- Documentación oficial de la librería `elm-csv`: https://package.elm-lang.org/packages/elm/csv/latest/
- Ejemplos de uso de `elm-csv` en proyectos reales: https://github.com/elm-csv/examples