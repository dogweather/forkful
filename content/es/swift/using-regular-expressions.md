---
title:    "Swift: Utilizando expresiones regulares"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar expresiones regulares en programación Swift

En el mundo de la programación, existen diferentes herramientas y técnicas que nos permiten trabajar de manera más eficiente y efectiva. Una de ellas son las expresiones regulares, las cuales tienen una gran utilidad al momento de realizar búsquedas y manipulación de cadenas de texto. En este blog post, exploraremos por qué deberías considerar incorporar expresiones regulares en tus proyectos de programación en Swift.

## Cómo utilizar expresiones regulares en programación Swift

Para utilizar expresiones regulares en Swift, primero debemos importar la librería de Foundation, la cual es la encargada de proporcionarnos las funciones necesarias para trabajar con texto. Luego, debemos crear una instancia de la clase NSRegularExpression, la cual nos permitirá definir nuestro patrón de búsqueda. Por ejemplo, si queremos buscar todas las palabras que empiecen con la letra "a" en una cadena, podemos usar el siguiente código:

```Swift
import Foundation

let str = "Un avión aterriza en el aeropuerto a las 3am"
let regex = try NSRegularExpression(pattern: "\\ba\\w*\\b")
let matches = regex.matches(in: str, options: [], range: NSRange(location: 0, length: str.utf16.count))

for match in matches {
    let word = String(str[Range(match.range, in: str)!])
    print(word) // Imprime "avión", "aeropuerto", "am"
}
```

Como se puede ver, al utilizar expresiones regulares podemos realizar búsquedas muy específicas en cadenas de texto de manera rápida y sencilla.

## Profundizando en el uso de expresiones regulares en Swift

Además de la función de búsqueda, las expresiones regulares también nos permiten realizar sustituciones de texto utilizando el método stringByReplacingMatches. También podemos utilizar los caracteres de anclaje "^" (inicio de línea) y "$" (fin de línea) para buscar patrones en líneas específicas de un texto. Además, podemos utilizar los operadores "OR" ("|") y "AND" ("&") para realizar búsquedas más complejas.

Otra característica interesante de las expresiones regulares en Swift es la posibilidad de utilizar grupos de captura, los cuales nos permiten extraer partes específicas de una cadena de texto. Por ejemplo, si queremos extraer el código de área de un número de teléfono, podemos utilizar el siguiente código:

```Swift
let str = "Mi número de teléfono es (555)123-4567"
let regex = try NSRegularExpression(pattern: "\\((\\d{3})\\)\\d{3}-\\d{4}")
let match = regex.firstMatch(in: str, options: [], range: NSRange(location: 0, length: str.utf16.count))
let areaCode = String(str[Range(match!.range(at: 1), in: str)!]) // Imprime "555" 
```

Como podemos ver, las expresiones regulares nos permiten realizar tareas muy específicas en un texto de manera muy flexible.

## Ver también

- [Uso de expresiones regulares en Swift](https://nshipster.com/nsregularexpression/)
- [Documentación oficial de expresiones regulares en Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Expresiones regulares para principiantes](https://medium.com/@vaibullah/regular-expressions-for-beginners-9d54f50f8b04)