---
date: 2024-01-26 01:11:47.870517-07:00
description: "C\xF3mo hacerlo: Imagina una tarea: calcular el promedio de un arreglo.\
  \ Sin funciones, lo pondr\xEDas todo en el main. Con funciones, har\xEDas esto."
lastmod: '2024-04-05T22:38:59.823808-06:00'
model: gpt-4-1106-preview
summary: "Imagina una tarea: calcular el promedio de un arreglo. Sin funciones, lo\
  \ pondr\xEDas todo en el main. Con funciones, har\xEDas esto."
title: "Organizando c\xF3digo en funciones"
weight: 18
---

## Cómo hacerlo:
Imagina una tarea: calcular el promedio de un arreglo. Sin funciones, lo pondrías todo en el main. Con funciones, harías esto:

```swift
func calcularPromedio(de numeros: [Double]) -> Double {
    let suma = numeros.reduce(0, +)
    return numeros.isEmpty ? 0 : suma / Double(numeros.count)
}

// Uso
let calificaciones = [92.5, 88.75, 99.0, 70.5]
let promedioCalificaciones = calcularPromedio(de: calificaciones)
print("El promedio de las calificaciones es \(promedioCalificaciones)")
```

La salida del ejemplo sería:
```
El promedio de las calificaciones es 87.6875
```

## Estudio detallado
Históricamente, a medida que la programación se volvió compleja, las funciones se convirtieron en una piedra angular para manejar la complejidad. Alternativas incluyen la codificación en línea y copiar y pegar código (código espagueti), que ahora se considera en gran medida una mala práctica. En Swift, las funciones son ciudadanos de primera clase; se pueden asignar a variables, pasar como argumentos y devolver desde otras funciones, lo que hace que el código sea más modular y flexible.

En cuanto a la implementación, diseña tus funciones para que hagan una cosa bien. Apunta a funciones con un propósito claro y un nombre que lo refleje. Ten en cuenta la cantidad de parámetros: si son demasiados, probablemente estás haciendo demasiado. ¿Manejo de errores? Considera las funciones que lanzan excepciones y maneja los problemas de manera elegante. Recuerda: Swift trata sobre la legibilidad y facilidad de mantenimiento.

## Ver también
- [Guía del lenguaje de programación Swift - Funciones](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Guía de estilo de Swift de Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
- [Refactoring: Mejorar el diseño del código existente, de Martin Fowler](https://martinfowler.com/books/refactoring.html)
