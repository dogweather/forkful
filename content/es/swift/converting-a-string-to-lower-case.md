---
title:                "Swift: Convirtiendo una cadena en minúsculas"
simple_title:         "Convirtiendo una cadena en minúsculas"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una cadena de texto a minúsculas es una tarea común en el desarrollo de aplicaciones, especialmente cuando se trabaja con datos de entrada del usuario. Es importante saber cómo hacerlo correctamente para asegurarse de que el programa funcione como se espera.

## Cómo hacerlo

Para convertir una cadena de texto a minúsculas en Swift, podemos utilizar el método `lowercased()` de la clase `String`. Este método devuelve una nueva cadena con todos los caracteres en minúsculas.

```Swift
let texto = "Hola Mundo"
let textoEnMinusculas = texto.lowercased()
print(textoEnMinusculas) // salida: hola mundo
```

Podemos aplicar este método a cualquier cadena de texto, ya sea una variable o un texto literal.

```Swift
let nombre = "Juan"
let nombreMinusculas = nombre.lowercased()
print(nombreMinusculas) // salida: juan
```

También podemos encadenar métodos para realizar múltiples tareas con una cadena de texto.

```Swift
let mensaje = "MIRANDO LAS ESTRELLAS"
let mensajeFormateado = mensaje.lowercased().capitalized
print(mensajeFormateado) // salida: Mirando las estrellas
```

## Profundizando

Es importante tener en cuenta que el método `lowercased()` solo convierte los caracteres que tienen una equivalencia de minúsculas en el sistema de codificación de caracteres utilizado. Esto significa que si tenemos un carácter con acento o alguna otra marca específica de un idioma, es posible que no se convierta a minúscula correctamente.

Por ejemplo, en español tenemos la letra "Ñ", que en mayúsculas se representa como "Ñ" y en minúsculas como "ñ". Si intentamos convertir un texto que contenga esta letra utilizando `lowercased()`, el resultado no será el esperado.

```Swift
let palabra = "AÑOS"
let palabraEnMinusculas = palabra.lowercased()
print(palabraEnMinusculas) // salida: aÑOS
```

Para solucionar este problema, podemos utilizar el método `folding(options:locale:)` de la clase `String`, que convierte los caracteres a su forma equivalente sin acentos ni diacríticos.

```Swift
let palabra = "AÑOS"
let palabraEnMinusculas = palabra.folding(options: .diacriticInsensitive, locale: .current).lowercased()
print(palabraEnMinusculas) // salida: años
```

## Ver también

- [Documentación oficial de Swift sobre la clase `String`](https://developer.apple.com/documentation/swift/string)
- [Método `lowercased()` en la documentación de Swift](https://developer.apple.com/documentation/swift/string/3126819-lowercased)
- [Método `folding(options:locale:)` en la documentación de Swift](https://developer.apple.com/documentation/swift/string/2018634-folding)