---
title:                "Swift: Borrando caracteres que coinciden con un patrón"
simple_title:         "Borrando caracteres que coinciden con un patrón"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué

A veces, al escribir un programa en Swift, nos podemos encontrar con la necesidad de eliminar ciertos caracteres que coinciden con un patrón específico. Esto puede ser útil en diferentes situaciones, como por ejemplo, limpiar una cadena de texto antes de utilizarla en una aplicación.

## Cómo hacerlo

Para eliminar caracteres que coinciden con un patrón en Swift, podemos utilizar la función `replacingOccurrences(of:with:)`. Esta función toma dos parámetros: el patrón que queremos buscar y el carácter con el que queremos reemplazarlo.

Por ejemplo, si queremos eliminar todos los números de una cadena de texto, podemos utilizar la siguiente línea de código:

```Swift
let texto = "3 manzanas y 4 naranjas"
let textoSinNumeros = texto.replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression)
print(textoSinNumeros) // "manzanas y naranjas"
```

En este ejemplo, estamos utilizando una expresión regular para buscar todos los números del 0 al 9 en la cadena de texto y reemplazarlos por una cadena vacía.

También podemos utilizar la función `filter` de Swift para eliminar caracteres que coinciden con un patrón. Esta función acepta un bloque de código que especifica las condiciones que deben cumplir los caracteres para ser eliminados.

Por ejemplo, si queremos eliminar todas las mayúsculas de una cadena de texto, podemos utilizar el siguiente código:

```Swift
let texto = "Hola Mundo"
let textoSinMayusculas = texto.filter { !($0.isUppercase) }
print(textoSinMayusculas) // "la undo"
```

En este caso, estamos utilizando el método `filter` para recorrer cada carácter de la cadena de texto y eliminar aquellos que sean mayúsculas.

## Profundizando

La eliminación de caracteres que coinciden con un patrón puede ser muy útil en situaciones en las que necesitamos limpiar datos antes de utilizarlos en nuestra aplicación. Por ejemplo, podemos utilizarlo para validar correos electrónicos, nombres de usuario o contraseñas. También puede ser útil para eliminar caracteres no deseados de una cadena de texto, como emojis o símbolos extraños.

Es importante tener en cuenta que, al utilizar expresiones regulares, debemos asegurarnos de que el patrón que estamos buscando es específico y no coincida con más caracteres de los deseados. De lo contrario, podríamos eliminar información importante de nuestra cadena de texto.

## Ver también

- [Documentación oficial de Swift - Función replacingOccurrences](https://developer.apple.com/documentation/foundation/nsstring/1411946-replacingoccurrences)
- [Documentación oficial de Swift - Método filter](https://developer.apple.com/documentation/swift/array/3018285-filter)