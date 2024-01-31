---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con CSV (valores separados por comas) significa manipular datos en un formato de texto simple que se usa ampliamente porque es fácil de leer y escribir. Los programadores utilizan CSV para intercambiar datos con sistemas que puedan no manejar formatos más complejos como JSON o XML.

## Cómo hacerlo:

En Elm, podemos usar paquetes como `elm-csv` para decodificar CSV. Aquí hay un ejemplo sencillo:

```Elm
import Csv

csvData : String
csvData =
    "name,age\nAlice,30\nBob,25"

type alias Person =
    { name : String, age : Int }

decodeCsv : String -> Result String (List Person)
decodeCsv data =
    Csv.decode data
        |> Csv.withHeader
        |> Csv.toDecoder (Csv.map2 Person (Csv.field "name") (Csv.field "age" Csv.int))

-- Uso:
case decodeCsv csvData of
    Ok people ->
        -- Haz algo con la lista de `Person`

    Err errorMessage ->
        -- Maneja el error
```

Ahora, si ejecutas la función `decodeCsv` con `csvData`, obtendrás `Ok [ Person "Alice" 30, Person "Bob" 25 ]`.

## Análisis Profundo:

CSV tiene sus raíces en la década de 1970, cuando los datos comenzaron a almacenarse y transferirse electrónicamente. A pesar de su antigüedad, es todavía muy relevante. Sin embargo, hay alternativas, como JSON o XML, que ofrecen estructuras de datos más ricas. Implementar la decodificación de CSV en Elm requiere entender bien cómo manejar strings y convertirlos en estructuras de datos útiles, lo que a veces puede ser complejo.

## Ver También:

- [elm-csv documentación](https://package.elm-lang.org/packages/lovasoa/elm-csv/latest/)
- [Guía sobre JSON en Elm](https://package.elm-lang.org/packages/elm/json/latest/)

Es útil tener a mano estas fuentes para profundizar en la manipulación de CSV y sus alternativas en Elm y otros lenguajes de programación.
