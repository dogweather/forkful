---
title:                "Concatenación de cadenas de texto"
aliases:
- /es/swift/concatenating-strings.md
date:                  2024-01-20T17:35:47.845237-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenación de cadenas de texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (¿Qué y Por Qué?)
Concatenar cadenas significa unir dos o más strings en uno solo. Hacemos esto para construir mensajes, combinar datos o simplemente para formatear la salida de un modo más legible.

## How to: (Cómo hacerlo:)
Swift simplifica la concatenación de cadenas. Aquí tienes diferentes formas de hacerlo:

```Swift
// Usando el operador '+'
let greeting = "Hola, "
let name = "Mundo"
let welcome = greeting + name
print(welcome) // Imprime "Hola, Mundo"

// Usando la interpolación de strings
let exclamation = "!"
let excitedWelcome = "\(greeting)\(name)\(exclamation)"
print(excitedWelcome) // Imprime "Hola, Mundo!"

// Usando el método append()
var message = "Hola"
message.append(", Mundo")
print(message) // Imprime "Hola, Mundo"
```

## Deep Dive (Profundizando)
La concatenación de cadenas no es nada nuevo; lleva siendo un concepto básico en la programación desde el principio. Alternativas a la concatenación directa incluyen el uso de arrays y su método `.join()`, que puede ser más eficiente en casos de múltiples cadenas. Al concatenar strings en Swift, el compilador optimiza la operación detrás de escena.

Swift también maneja la concatenación de forma segura según el tipo, es decir, solo permite la concatenación entre strings, de este modo se reducen errores en tiempo de ejecución. Además, la facilidad de uso y sintaxis clara de Swift hacen de la concatenación de cadenas una tarea sin complicaciones.

## See Also (Consulta También)
- Documentación oficial de Swift sobre Strings: [Swift String Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Tutorial de Swift sobre la manipulación de Strings: [Ray Wenderlich String Tutorial](https://www.raywenderlich.com/553-string-manipulation-in-swift-4)
- Swift Standard Library en String: [Swift Standard Library - String](https://developer.apple.com/documentation/swift/string)
