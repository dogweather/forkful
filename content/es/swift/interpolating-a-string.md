---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué es y por qué?

La interpolación de cadenas en Swift te permite insertar datos directamente dentro de una cadena. Es útil para agregar variables o expresiones a tus cadenas de texto sin usar una concatenación engorrosa.

## Cómo hacerlo:

La interpolación de cadenas en Swift es asombrosamente sencilla.

```Swift
let nombre = "Miguel"
let saludo = "¡Hola, \(nombre)!"
print(saludo)
```

El código anterior imprimirá: "¡Hola, Miguel!". Como ves, encapsulamos la variable `nombre` en paréntesis y añadimos una barra invertida `\` justo antes.

## Profundizando:

Si te interesa, aquí hay un poco de historia, opciones alternativas y detalles de la implementación de la interpolación de cadenas en Swift.

1. **Contexto histórico**: La interpolación de cadenas data de los lenguajes de programación clásicos como Perl y ha sido adoptada en muchos lenguajes modernos, incluyendo Swift.

2. **Alternativas**: Podrías usar la concatenación de cadenas, pero ese método tiende a ser más torpe y difícil de leer.

3. **Detalles de implementación**: Cuando trabajas con interpolación de cadenas, Swift hace un trabajo interno preparando y formateando tu cadena para que puedas incorporar valores directamente. Esto puede tener implicaciones en términos de rendimiento en situaciones con muchas cadenas a interpolar, aunque en la mayoría de los casos, la diferencia será mínima.

## Ver también:

Mira estos recursos adicionales para aprender más sobre la interpolación de cadenas en Swift:

1. [Documentación oficial de Swift sobre la interpolación de cadenas](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
2. [Tutorial detallado sobre la interpolación de cadenas en Swift](https://www.hackingwithswift.com/quick-start/understanding-swift/how-to-add-interpolated-variables-into-a-string)

Nunca está de más aprender más detalles sobre las herramientas que usas día a día, ¡feliz programación!