---
title:                "Swift: Utilizando expresiones regulares"
simple_title:         "Utilizando expresiones regulares"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué usar expresiones regulares en Swift

Las expresiones regulares son una herramienta útil para trabajar con patrones de texto en Swift. Permiten buscar, reemplazar y extraer información específica dentro de un bloque de texto. En este artículo, exploraremos cómo usar expresiones regulares en Swift y profundizaremos en su funcionalidad.

## Cómo usar expresiones regulares en Swift

Para trabajar con expresiones regulares en Swift, necesitaremos importar el framework `Foundation`. Luego, podemos utilizar la clase `NSRegularExpression` para crear una expresión regular y el método `matches(in:options:range:)` para buscar coincidencias en un texto determinado.

Veamos un ejemplo de código para buscar correos electrónicos en un texto:

```Swift
import Foundation

let texto = "¡Hola! Mi dirección de correo electrónico es ejemplo@email.com. ¿Cuál es la tuya?"
let expresionRegular = try! NSRegularExpression(pattern: "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}", options: .caseInsensitive)
let coincidencias = expresionRegular.matches(in: texto, options: [], range: NSMakeRange(0, texto.utf16.count))

print(coincidencias) // [16..<33]
```

En este ejemplo, la expresión regular utilizada buscará un patrón de texto que sea parecido a una dirección de correo electrónico. La variable `coincidencias` nos devolverá un rango en el que se encuentra la coincidencia dentro del texto. En este caso, el rango es desde el índice 16 hasta el 33, lo que nos indica que encontró la dirección de correo electrónico en esa posición.

Podemos utilizar este mismo método para buscar otros patrones de texto, como números de teléfono, direcciones, nombres, entre otros. Solo necesitaremos modificar la expresión regular para que coincida con el patrón deseado.

## Profundizando en el uso de expresiones regulares en Swift

Mientras más conocimiento tengamos sobre las expresiones regulares, más precisas y específicas podrán ser nuestras búsquedas. Algunos de los conceptos que debemos conocer para utilizar adecuadamente las expresiones regulares son:

- **Caracteres comodín:** los comodines nos permiten buscar patrones de texto específicos. Por ejemplo, el punto (`.`) es un comodín que representa cualquier carácter, mientras que el asterisco (`*`) representa cero o más ocurrencias del carácter anterior.
- **Grupos de caracteres:** los grupos de caracteres nos permiten definir un conjunto de caracteres que queremos buscar. Por ejemplo, `[a-z]` buscará cualquier carácter entre la `a` y la `z`.
- **Cuantificadores:** los cuantificadores nos permiten especificar la cantidad de ocurrencias del patrón que queremos buscar. Por ejemplo, `{3}` buscará exactamente tres ocurrencias del patrón anterior.
- **Modificadores:** los modificadores nos permiten especificar opciones adicionales para nuestra búsqueda, como ignorar mayúsculas y minúsculas o buscar en varias líneas.

Es importante aprender sobre estos y otros conceptos de expresiones regulares para poder utilizarlas de manera efectiva en nuestro código de Swift.

## Consulta estos recursos para aprender más sobre expresiones regulares en Swift

Si deseas profundizar aún más en el uso de expresiones regulares en Swift, te recomendamos consultar estos recursos:

- [Documentación oficial de Apple sobre expresiones regulares en Swift](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Tutorial de Ray Wenderlich sobre expresiones regulares en Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)
- [Guía completa de expresiones regulares en Swift](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID307)

## Ver también

- [Tutorial de Markdown para principiantes](https://www.makeuseof.com/tag/markdown-how-to-write-for-the-web/)
- [Documentación oficial de Markdown](https://www.markdownguide.org/)