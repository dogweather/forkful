---
title:                "Conversión de una cadena de texto a minúsculas"
aliases:
- /es/swift/converting-a-string-to-lower-case/
date:                  2024-01-20T17:39:09.154569-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversión de una cadena de texto a minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Convertir una cadena de texto (string) a minúsculas significa transformar todos los caracteres alfabéticos en su versión en minúscula. Los programadores hacen esto para homogeneizar datos, simplificar comparaciones de texto y mejorar la consistencia en operaciones de búsqueda y entrada de datos.

## Cómo hacerlo:
Swift hace que pasar a minúsculas sea pan comido con el método `lowercased()`. Aquí tienes un ejemplo:

```swift
let originalString = "Hola Mundo!"
let lowercasedString = originalString.lowercased()
print(lowercasedString) // "hola mundo!"
```

Muestra el resultado: `hola mundo!`

## Profundización:
Convertir cadenas a minúsculas no es nuevo, está desde los primeros días de la programación. En Swift, `lowercased()` es simple y efectivo, pero hay par de cosas a considerar:

1. **Contexto Histórico**: Antes, lenguajes como C requerían bucles para cambiar carácter por carácter. Swift lo simplifica.
   
2. **Alternativas**: Además de `lowercased()`, podrías usar funciones de manejo de texto más complejas de Foundation o incluso Cocoa para manipulaciones específicas de localización.
   
3. **Detalles de Implementación**: `lowercased()` considera la configuración regional actual para transformar caracteres específicos correctamente. Para un comportamiento más estándar, `lowercased(with: Locale)` permite especificar una configuración regional.

## Ver También:
Para más detalles, revisa los siguientes enlaces:

- Documentación oficial de Swift sobre String: [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- Un tutorial sobre cómo las cadenas funcionan en Swift: [Swift Strings](https://www.raywenderlich.com/5539282-strings-and-string-interpolation-in-swift)
