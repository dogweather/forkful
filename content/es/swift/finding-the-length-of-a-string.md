---
title:                "Encontrando la longitud de una cadena."
html_title:           "Swift: Encontrando la longitud de una cadena."
simple_title:         "Encontrando la longitud de una cadena."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
En programación, conocer el largo de una cadena de caracteres es una habilidad importante ya que nos permite manipular y modificar la información de manera adecuada. Al conocer la longitud de una cadena, podemos asegurarnos de que los datos se almacenen de manera correcta y evitar errores en nuestro código.

## Cómo hacerlo:
Para encontrar la longitud de una cadena en Swift, podemos utilizar la propiedad `count` o el método `count` de la estructura `String`. Veamos un ejemplo práctico:

```Swift
let nombre = "Juan"
print(nombre.count) // resultado: 4
print(nombre.count + 2) // resultado: 6
```

## Profundizando:
El contar la longitud de una cadena es una técnica común en cualquier lenguaje de programación. En Swift, podemos usar la propiedad `count` para obtener el número de caracteres en una cadena. También podemos utilizar el método `count` si queremos realizar alguna operación con ese valor.

Otra alternativa para contar la longitud de una cadena es utilizando el método `countElements()`, pero se recomienda utilizar `count` ya que `countElements()` está siendo deprecado en versiones antiguas de Swift.

Es importante mencionar que en Swift, los caracteres `"á", "é", "í", "ó", "ú"` entre otros, son considerados como un solo elemento y no como dos, a diferencia de otros lenguajes de programación.

## Ver también:
- Documentación oficial de Swift sobre la estructura `String`: https://developer.apple.com/documentation/swift/string
- Otros métodos y propiedades de la estructura `String`: https://www.hackingwithswift.com/example-code/strings/how-to-loop-over-strings
- Uso del método `count` para validar la longitud de una cadena en la base de datos de Firebase: https://firebase.google.com/docs/firestore/query-data/get-data#example_get_all_documents_in_a_collection