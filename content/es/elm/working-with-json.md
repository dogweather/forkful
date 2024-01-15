---
title:                "Trabajando con json"
html_title:           "Elm: Trabajando con json"
simple_title:         "Trabajando con json"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con JSON en Elm?

JSON (JavaScript Object Notation) es un formato ligero y fácil de entender para el intercambio de datos. Al trabajar con Elm, un lenguaje de programación funcional y declarativo, el uso de JSON es beneficioso ya que nos permite manejar y manipular datos de manera eficiente y segura.

## Cómo hacerlo

Para trabajar con JSON en Elm, primero debemos importar el módulo `Json.Decode` en nuestro archivo. Este módulo nos proporciona funciones para convertir datos en formato JSON a tipos de datos nativos de Elm.

```Elm
import Json.Decode exposing (..)
```

Una vez que tenemos el módulo importado, podemos utilizar la función `decodeValue` para convertir nuestro JSON en un tipo de dato Elm específico.

```Elm
decodeValue : Decoder a -> Value -> Result String a
```

Aquí, `a` representa el tipo de dato que queremos obtener. Por ejemplo, si nuestro JSON es una lista de objetos que representan libros, podríamos utilizar un `Decoder` para convertirlos en un tipo de dato `List Book` (donde `Book` es un tipo de dato personalizado que representa un libro).

```Elm
type alias Book =
  { title : String
  , author : String
  , year : Int
  }

bookDecoder : Decoder Book
bookDecoder =
  map3 Book
    (field "title" string)
    (field "author" string)
    (field "year" int)

jsonString = """
[
  { "title": "1984", "author": "George Orwell", "year": 1949 },
  { "title": "To Kill a Mockingbird", "author": "Harper Lee", "year": 1960 },
  { "title": "Pride and Prejudice", "author": "Jane Austen", "year": 1813 }
]
"""

books = jsonString |> decodeString (list bookDecoder)
```

El resultado de `books` sería una lista de libros, donde cada elemento es un valor del tipo `Book`.

## Profundizando

Además de las funciones `decodeValue` y `decodeString`, el módulo `Json.Decode` también proporciona otras funciones útiles para trabajar con JSON en Elm, como `map`, `andThen` y `oneOf` que nos permiten manejar errores y casos especiales de forma elegante y segura.

Además, también podemos utilizar el módulo `Json.Encode` para convertir nuestros tipos de datos Elm en formato JSON, lo que nos permite enviar y recibir datos desde una API o servidor externo.

Para obtener más información y detalles sobre cómo trabajar con JSON en Elm, se recomienda consultar la documentación oficial y ejemplos de código.

## Ver también

- Documentación oficial de Elm sobre JSON: https://package.elm-lang.org/packages/elm/json/latest/
- Ejemplos de código: https://github.com/elm/json/tree/1.1.3/examples