---
title:                "Buscando y reemplazando texto"
html_title:           "C: Buscando y reemplazando texto"
simple_title:         "Buscando y reemplazando texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Búsqueda y Reemplazo de Texto en Swift

## ¿Qué & Por Qué?

Buscar y reemplazar texto son operaciones comunes en programación. Nos permiten mantener y reestructurar gran cantidad de contenido textual con mínimo esfuerzo.

## Cómo se hace:

Aquí te dejamos un ejemplo básico de cómo buscar y reemplazar texto con Swift.

En Swift, usamos el método `replacingOccurrences(of:with:)` para reemplazar texto:

```Swift
let textoOriginal = "Hola, mundo!"
let textoModificado = textoOriginal.replacingOccurrences(of: "mundo", with: "Swift")
print(textoModificado)
```

Este código imprimirá:

```Swift
Hola, Swift!
```

## Análisis Profundo

Historia: Swift introdujo el método `replacingOccurrences(of:with:)` desde su lanzamiento, lo que facilitó el proceso de búsqueda y reemplazo.

Alternativas: Podrías usar expresiones regulares en Swift para lograr resultados más complejos, aunque eso implicaría aprender otro lenguaje de programación.

Detalles de la implementación: `replacingOccurrences(of:with:)` funciona realizando una búsqueda en todo el texto para encontrar cada ocurrencia de la cadena de búsqueda, y luego reemplazar cada ocurrencia con la cadena de reemplazo.

## Ver También

2. [Una introducción a las expresiones regulares en Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift).
3. [Un video tutorial](https://www.youtube.com/watch?v=HZ7TOJ61V70) sobre cómo usar el método `replacingOccurrences(of:with:)`.