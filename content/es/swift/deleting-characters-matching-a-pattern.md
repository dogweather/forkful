---
title:    "Swift: Borrando caracteres que coinciden con un patrón"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por qué
En programación Swift, a veces es necesario eliminar ciertos caracteres que coinciden con un patrón específico en una cadena de texto. Esto puede ser útil en situaciones como eliminar espacios en blanco alrededor de una palabra o eliminar símbolos no deseados de una entrada de usuario.

## Cómo hacerlo
Para eliminar caracteres que coinciden con un patrón en Swift, podemos utilizar el método `replacingOccurrences(of:with)` en la cadena de texto que queremos modificar. En este método, especificamos el patrón que queremos buscar y el carácter con el que queremos reemplazarlo. Aquí hay un ejemplo de cómo eliminar los espacios en blanco alrededor de una palabra en una cadena:

```Swift
let texto = " Hola, ¿cómo estás? "
let nuevoTexto = texto.replacingOccurrences(of: " ", with: "")
print(nuevoTexto) // Salida: "Hola,¿cómoestás?"
```

También podemos usar expresiones regulares en lugar de un patrón específico para buscar y eliminar ciertos caracteres. Por ejemplo, si queremos eliminar todos los números de una cadena de texto, podemos usar una expresión regular para buscar cualquier dígito y reemplazarlo con una cadena vacía:

```Swift
let texto = "¡Tengo 3 manzanas y 5 naranjas!"
let nuevoTexto = texto.replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression)
print(nuevoTexto) // Salida: "¡Tengo manzanas y naranjas!"
```

## Profundizando más
Existen diversas formas de utilizar el método `replacingOccurrences(of:with)` para eliminar caracteres que coinciden con un patrón en Swift. Se pueden agregar diversas opciones, como realizar la búsqueda y el reemplazo de forma insensible a mayúsculas y minúsculas o limitar el número de reemplazos a realizar. Se pueden consultar más detalles en la documentación oficial de Apple sobre este método.

## Ver también
- Documentación de Apple sobre el método `replacingOccurrences(of:with:)`: https://developer.apple.com/documentation/foundation/nsstring/1416926-replacingoccurrences
- Información sobre el uso de expresiones regulares en Swift: https://www.hackingwithswift.com/example-code/strings/how-to-use-regular-expressions-in-swift
- Un tutorial para aprender más sobre las expresiones regulares: https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial