---
date: 2024-01-26 04:45:25.154449-07:00
description: "Los n\xFAmeros complejos tienen una parte real y una parte imaginaria\
  \ (como 3 + 4i). Los programadores los usan en Swift para tareas como el procesamiento\
  \ de\u2026"
lastmod: '2024-02-25T18:49:55.880392-07:00'
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos tienen una parte real y una parte imaginaria (como\
  \ 3 + 4i). Los programadores los usan en Swift para tareas como el procesamiento\
  \ de\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## Qué y Por Qué
Los números complejos tienen una parte real y una parte imaginaria (como 3 + 4i). Los programadores los usan en Swift para tareas como el procesamiento de señales, la solución de ciertos problemas matemáticos y la simulación de la física.

## Cómo hacerlo:
Swift no tiene soporte integrado para números complejos, pero podemos crear el nuestro:

```Swift
struct NumeroComplejo {
    var real: Double
    var imaginario: Double
    
    func add(_ otro: NumeroComplejo) -> NumeroComplejo {
        return NumeroComplejo(real: real + otro.real, imaginario: imaginario + otro.imaginario)
    }
    
    // Métodos adicionales como la sustracción, multiplicación, etc.
}

let primero = NumeroComplejo(real: 2, imaginario: 3)
let segundo = NumeroComplejo(real: 1, imaginario: 4)
let resultado = primero.add(segundo)
print("Resultado: \(resultado.real) + \(resultado.imaginario)i")
// Salida de muestra: Resultado: 3.0 + 7.0i
```

## Análisis Profundo
Los números complejos aparecieron en el siglo XVI en ecuaciones algebraicas. Son esenciales en mecánica cuántica, teoría de control y muchos otros campos. Swift de Apple no tiene una biblioteca estándar para números complejos, a diferencia de lenguajes como Python o C++. Las alternativas para crear el propio incluyen usar el paquete Numérico que incluye soporte para números complejos o envolver la biblioteca compleja de C++ con la interoperabilidad de Swift.

## Ver También
- Swift Numéricos: [https://github.com/apple/swift-numerics](https://github.com/apple/swift-numerics)
