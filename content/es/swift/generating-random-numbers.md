---
title:                "Generando números aleatorios"
date:                  2024-01-20T17:50:10.365021-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generando números aleatorios"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generar números aleatorios es simplemente obtener valores que no siguen un patrón predecible. Los programadores los utilizan para todo, desde juegos y simulaciones hasta seguridad y análisis de datos. Es la salsa secreta para la "aleatoriedad" en la programación.

## How to:
Swift hace que trabajar con números aleatorios sea pan comido. Mira estos ejemplos:

```Swift
// Obtener un número entero aleatorio entre 0 y 10.
let randomInt = Int.random(in: 0...10)
print(randomInt)  // Ejemplo de salida: 7

// Generar un número flotante aleatorio entre 0 y 1.
let randomFloat = Float.random(in: 0..<1)
print(randomFloat)  // Ejemplo de salida: 0.123456

// Crear un número aleatorio de doble precisión entre -1 y 1.
let randomDouble = Double.random(in: -1...1)
print(randomDouble)  // Ejemplo de salida: -0.89123
```
Usa `random(in:)` con el rango deseado y ¡voilà! Números aleatorios al instante.

## Deep Dive:
Generar números aleatorios tiene una larga historia en la informática, remontándose a las loterías y juegos de azar. En Swift, `Random` se introdujo en Swift 4.2 como parte de la librería estándar, reemplazando métodos más antiguos y menos intuitivos.

Se utilizan algoritmos complejos, como arc4random en sistemas más antiguos y métodos más seguros en la tecnología actual, para asegurar que los números sean realmente "aleatorios", dentro de lo que la computación lo permite.

Alternativas a `random(in:)` podrían ser el uso de generadores de números pseudoaleatorios personalizados o librerías de terceros, especialmente si necesitas un tipo de aleatoriedad muy específico.

## See Also:
Para más información, puedes consultar estos enlaces:

- [Apple's Swift Documentation on Random Numbers](https://developer.apple.com/documentation/swift/randomnumbergenerator)

Estos recursos adicionales te proporcionarán un contexto más amplio y ejemplos prácticos para implementar números aleatorios en tus propios proyectos Swift.