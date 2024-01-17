---
title:                "Buscando y reemplazando texto"
html_title:           "Swift: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Buscar y reemplazar texto es una práctica común entre los programadores en la que se busca una determinada cadena de texto y luego se la reemplaza con otra. Esto es útil para realizar cambios en el código de manera rápida y eficiente. Además, puede ayudar a corregir errores o actualizar una parte del código en múltiples lugares.

## Cómo hacerlo:
```Swift
// Buscar y reemplazar "Hello" con "Hola" en una cadena de texto
let mensaje = "Hello, world!"
let mensajeReemplazado = mensaje.replacingOccurrences(of: "Hello", with: "Hola")
print(mensajeReemplazado)
// Output: "Hola, world!"
```

## Profundizando:
Buscar y reemplazar texto ha sido una función básica en la mayoría de los editores de texto desde hace décadas, y ahora también se utiliza ampliamente en lenguajes de programación. Además de la función `replacingOccurrences`, también existen otras formas de buscar y reemplazar texto en Swift, como `replacingCharacters` que permite reemplazar un carácter específico o `replacingRange` que permite reemplazar un rango de caracteres.

Para aquellos usuarios que prefieren interfaces gráficas, también existen herramientas externas que permiten buscar y reemplazar texto en múltiples archivos al mismo tiempo, como Sublime Text o Atom.

## Ver también:
- Documentación oficial de Swift sobre `replacingOccurrences`: https://developer.apple.com/documentation/swift/string/3126933-replacingoccurrences
- Comparación entre diferentes herramientas de búsqueda y reemplazo de texto: https://www.slant.co/topics/2178/~best-search-and-replace-tools