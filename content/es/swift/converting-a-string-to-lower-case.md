---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Swift: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Convertir una cadena de texto a letras minúsculas es un proceso común en la programación que significa modificar todas las letras mayúsculas de una cadena por sus equivalentes en minúsculas. Esto puede ser útil para comparar cadenas de texto de manera más precisa o para asegurar una consistencia en el formato del texto. Los desarrolladores suelen hacerlo para facilitar el manejo y manipulación de los datos en sus programas.

## Cómo:
```Swift
let cadena = "Este Es Un Ejemplo De Cadena"
print(cadena.lowercased()) // salida: este es un ejemplo de cadena
```

## Profundizando:
Este proceso de conversión de las letras mayúsculas a minúsculas se conoce como "lowercasing" y se ha utilizado en la programación desde los primeros días de los lenguajes de programación. En Swift, también existe el método `uppercased()` para convertir texto a letras mayúsculas, lo cual puede ser útil en ciertas situaciones.

Otra alternativa a `lowercased()` es utilizar la propiedad `caseInsensitiveCompare()` para comparar cadenas de texto sin tener en cuenta las mayúsculas y minúsculas. Sin embargo, esto podría ser menos eficiente en términos de rendimiento y no es recomendable para cadenas de texto muy largas.

En cuanto a la implementación, la función `lowercased()` utiliza el Unicode Standard para determinar el equivalente en minúsculas de cada letra en la cadena de texto. Esto asegura una conversión adecuada para diferentes idiomas y caracteres especiales.

## Vea También:
- [Documentación de Apple sobre el método `lowercased()`](https://developer.apple.com/documentation/swift/string/1789658-lowercased)
- [Comparación de cadenas de texto en Swift](https://www.hackingwithswift.com/articles/108/how-to-compare-strings-in-swift)