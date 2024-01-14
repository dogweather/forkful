---
title:                "Elm: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué

El trabajar con archivos CSV es una tarea muy común en el desarrollo de software. Ya sea para importar o exportar datos, mantener registros o realizar análisis, conocer cómo manejar archivos CSV en Elm puede ser muy útil para cualquier programador.

## Cómo

Para trabajar con archivos CSV en Elm, es necesario importar el módulo `Csv` de la siguiente manera:

```Elm
import Csv
```

Luego, para leer un archivo CSV, podemos utilizar la función `Csv.Decode.decode` que recibe un `Csv.Decoder` como argumento y devuelve una lista de filas del archivo.

```Elm
exampleCsv : String
exampleCsv =
"""
id,name,age
1,John,28
2,Lucia,32
3,Marco,25
"""

type alias Person =
    { id : Int
    , name : String
    , age : Int
    }

csvDecoder : Csv.Decoder ( List Person )
csvDecoder =
    Csv.Decode.list <|
        Csv.Decode.map3 Person
            (Csv.Decode.field "id" Csv.Decode.int)
            (Csv.Decode.field "name" Csv.Decode.string)
            (Csv.Decode.field "age" Csv.Decode.int)

people : List Person
people =
    Csv.Decode.decodeString csvDecoder exampleCsv
```

Con este código, podemos leer un archivo CSV y convertirlo en una lista de `Person` con sus respectivos campos.

## Deep Dive

Una de las ventajas de trabajar con archivos CSV en Elm es que podemos utilizar la validación de tipos para garantizar que los datos estén en el formato correcto. Esto se logra utilizando los decoders de `Csv` en conjunto con los decoders de `Json` para convertir los datos a tipos de datos personalizados.

Otra funcionalidad interesante es la capacidad de escribir archivos CSV utilizando la función `Csv.Encode.encode` pasándole una lista de filas y un `Csv.Encoder` que defina cómo se deberían escribir los datos.

## Ver también

Para obtener más información sobre cómo trabajar con archivos CSV en Elm, aquí hay algunos recursos útiles:

- Documentación oficial de Elm CSV: https://package.elm-lang.org/packages/elm-explorations/csv/latest/
- Ejemplos de código para leer y escribir archivos CSV: https://github.com/elm-explorations/csv/tree/master/examples
- Artículo sobre cómo usar tipos de datos personalizados con CSV: https://medium.com/@essenciary/using-custom-types-with-elm-s-csv-library-186ecaa05478