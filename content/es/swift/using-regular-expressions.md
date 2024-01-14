---
title:                "Swift: Utilizando expresiones regulares"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por qué utilizar Expresiones Regulares en Swift

Las expresiones regulares son una herramienta útil en cualquier lenguaje de programación, incluyendo Swift. Te permiten buscar y manipular patrones de texto de manera eficiente, ahorrándote tiempo y esfuerzo en tareas repetitivas. Sigue leyendo para descubrir cómo utilizarlas en tus proyectos en Swift.

## Cómo utilizar Expresiones Regulares en Swift

Para utilizar expresiones regulares en Swift, primero debes importar la librería `Foundation` en tu código.

```Swift
import Foundation
```

Luego, puedes crear una instancia de `NSRegularExpression`, pasando como parámetro la expresión regular que deseas utilizar.

```Swift
let regex = try! NSRegularExpression(pattern: "[0-9]+")
```

En este ejemplo, estamos creando una expresión regular que busca cualquier cadena de números en un texto.

Ahora, podemos utilizar este objeto `regex` para buscar coincidencias en una cadena de texto específica. Por ejemplo, si tenemos la siguiente cadena:

```Swift
let text = "Hola, mi número de teléfono es 1234567890"
```

Podemos utilizar la función `matches(in:options:range:)` de nuestro objeto `regex` para buscar coincidencias en el rango de texto que queremos.

```Swift
let matches = regex.matches(in: text, options: [], range: NSRange(location: 0, length: text.utf16.count))
```

En este caso, estamos buscando coincidencias en toda la cadena de texto `text`. La función nos devolverá un array de objetos `NSTextCheckingResult` que contienen información sobre las coincidencias encontradas.

Por ejemplo, si queremos imprimir todas las coincidencias encontradas en la consola, podemos utilizar un bucle `for` para recorrer el array de coincidencias:

```Swift
for match in matches {
    let matchRange = match.range
    let matchString = (text as NSString).substring(with: matchRange)
    print(matchString)
}
```

En este caso, el output impreso en la consola sería:

```
1234567890
```

## Un vistazo más profundo a las Expresiones Regulares en Swift

Existen varias funciones y métodos que puedes utilizar al trabajar con expresiones regulares en Swift. Por ejemplo, también puedes utilizar la función `numberOfMatches(in:options:range:)` para obtener el número de coincidencias encontradas en un rango de texto determinado.

Además, puedes utilizar diversos caracteres especiales en tus expresiones regulares, como `^` para buscar coincidencias al inicio de una cadena, o `$` para buscar coincidencias al final.

También puedes utilizar paréntesis para agrupar partes de tu expresión regular y utilizarlas en conjunto con otros caracteres especiales como `+` o `*`.

Para profundizar más en el uso de expresiones regulares en Swift, te recomendamos revisar la documentación oficial de Apple sobre la clase `NSRegularExpression` y experimentar con diferentes patrones y cadenas de texto.

## Ver también

- [Documentación oficial de Swift sobre `NSRegularExpression`](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Tutorial de uso de expresiones regulares en Swift](https://www.raywenderlich.com/825807-swift-regular-expressions-tutorial-cheat-sheet)
- [Cheat sheet con los caracteres especiales más comunes en expresiones regulares](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)