---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

---
## ¿Qué & Por qué?

La concatenación de cadenas en Swift es simplemente combinar dos o más cadenas de texto. Los programadores lo hacen para crear mensajes dinámicos, datos de salida, entre otras cosas.

## ¿Cómo hacerlo?

Aquí hay algunos ejemplos de cómo concatenar cadenas en Swift. 

```Swift
let cadena1 = "¡Hola, "
let cadena2 = "mundo!"
let frase = cadena1 + cadena2
print(frase)
// Resultado: ¡Hola, mundo!
```

También puedes usar la interpolación de cadenas para combinarlas:

```Swift
let nombre = "Juan"
let saludo = "¡Hola, \(nombre)!"
print(saludo)
// Resultado: ¡Hola, Juan!
```

O puedes usar el método `append` para añadir una cadena a otra:

```Swift
var hola = "¡Hola, "
hola.append("mundo!")
print(hola)
// Resultado: ¡Hola, mundo!
```

## Inmersión profunda

La concatenación de cadenas ha existido desde los primeros días de la programación, es una forma fundamental de manejar cadenas de texto. En Swift, existen varias maneras de hacerlo, cada una con sus propias ventajas y desventajas.

Las alternativas a la concatenación de cadenas incluyen el uso de `String(format:)` para un control más preciso de la salida de texto, pero con un costo en rendimiento y legibilidad.

Detalles de la implementación: En Swift, la concatenación de cadenas crea una nueva cadena en lugar de modificar una existente. Esto puede tener implicaciones en el rendimiento si estás concatenando un gran número de cadenas, ya que cada operación de concatenación requiere la asignación de una nueva cadena en memoria.

## Ver también

1. [Documentación oficial de Swift sobre cadenas y caracteres.](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)

2. [Swift.org - Interpolación de cadenas.](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)