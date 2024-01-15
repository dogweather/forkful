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

## Por qué
Todo programador en algún momento necesita buscar y reemplazar texto dentro de su código. Esto puede ser por errores ortográficos, cambios en la lógica del programa o simplemente para ahorrar tiempo al modificar varias líneas de código a la vez.

## Cómo hacerlo
La forma más sencilla de buscar y reemplazar texto en Swift es utilizando la función `replacingOccurrences(of:with:)`. Esta función toma dos parámetros, el texto que se desea reemplazar y el texto por el que se quiere reemplazar. A continuación, se muestra un ejemplo:

```Swift
var frase = "Me gusta programar en Swift"
frase = frase.replacingOccurrences(of: "Swift", with: "Python")

print(frase)
```

Este código imprimirá "Me gusta programar en Python", ya que la función ha reemplazado la palabra "Swift" por "Python" en la variable `frase`. Además, se pueden utilizar las opciones adicionales de la función, como especificar si se quiere ignorar las mayúsculas o minúsculas, o limitar el número de reemplazos realizados.

## Profundizando
En Swift también se puede utilizar la función `range(of:)` para buscar un rango específico de texto dentro de una cadena, y luego utilizar la función `replaceSubrange(_:with:)` para reemplazarlo. Esta opción es más útil cuando se quiere reemplazar únicamente una parte de una cadena y no todas las ocurrencias. A continuación, se muestra un ejemplo:

```Swift
var oracion = "Hoy es un hermoso día para programar en Swift"
let rango = oracion.range(of: "Hoy es un")
oracion.replaceSubrange(rango!, with: "Mañana será un")

print(oracion)
```

Este código imprimirá "Mañana será un hermoso día para programar en Swift", ya que ha reemplazado el texto "Hoy es un" por "Mañana será un" utilizando el rango específico obtenido con `range(of:)`.

## Ver también
- [Documentación oficial de Swift sobre la función de búsqueda y reemplazo](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#//apple_ref/doc/uid/TP40014097-CH7-SW9)
- [Más ejemplos de búsqueda y reemplazo en Swift](https://www.hackingwithswift.com/example-code/strings/how-to-replace-a-substring-with-another-substring)