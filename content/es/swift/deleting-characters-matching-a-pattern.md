---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Eliminar caracteres que coinciden con un patrón en Swift implica localizar y quitar caracteres específicos de una cadena de texto. Los programadores suelen hacerlo para limpiar o normalizar los datos de entrada.

## ¿Cómo...?

En Swift, usamos la función `replacingOccurrences(of:with:options:range:)` de la clase `String`. Aquí tienes un ejemplo detallado:

```Swift
let original = "Hola, ¿cómo estás?"
let filtrada = original.replacingOccurrences(of: "?", with: "")
print(filtrada)  // Resultado: "Hola, ¿cómo estás"
```

En este caso, hemos eliminado el carácter "?" que coincide con nuestro patrón de búsqueda.

## Análisis en Profundidad

Originariamente, las técnicas de manipulación de cadenas como ésta se originaron en el auge de la informática. A medida que los datos se volvían cada vez más importantes, la necesidad de limpiar y organizar los datos espoleó el desarrollo de estas funciones.
La alternativa a usar `replacingOccurrences(of:with:options:range:)` es crear tu propia función utilizando ciclos y comparaciones carácter por carácter, pero es más engorroso y menos eficiente.
La implementación de `replacingOccurrences(of:with:options:range:)` hace uso del framework Foundation de Apple, lo que demuestra la importancia de este para Swift. Este método emplea la potencia de las expresiones regulares para hacer coincidir los patrones.

## Ver También

Para un entendimiento más profundo sobre la manipulación de cadenas en Swift, consulta: 
1. La [documentación oficial de la clase `String`](https://developer.apple.com/documentation/swift/string) de Apple.
2. El [artículo](https://www.hackingwithswift.com/articles/141/8-useful-swift-extensions) de HackingWithSwift sobre extensiones útiles para cadenas en Swift.

Recuerda que la programación es continuo aprendizaje. ¡Que te diviertas codificando!