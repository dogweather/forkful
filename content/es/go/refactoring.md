---
title:                "Refactorización"
date:                  2024-01-26T01:18:19.328302-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactorización"

category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/refactoring.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?
Refactorizar es el proceso de reestructurar código de computadora existente sin cambiar su comportamiento externo. Los programadores lo hacen para mejorar atributos no funcionales del software, como la legibilidad y mantenibilidad, lo que puede hacer que el código sea más fácil de entender, reducir la complejidad y ayudar a detectar errores más fácilmente.

## Cómo hacerlo:
Vamos a sumergirnos en un simple ejemplo de refactorización de código en Go. Tomaremos un fragmento que calcula el promedio de un conjunto de números y lo refactorizamos para claridad y reusabilidad.

Código original:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Promedio:", average)
}
```

Código refactorizado:
```Go
package main

import "fmt"

// CalculateAverage toma un conjunto de float64 y devuelve el promedio.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Promedio:", average)
}
```

En el código refactorizado, hemos extraído la lógica que calcula el promedio a una función separada llamada `CalculateAverage`. Esto hace que la función `main` sea más concisa y la lógica de cálculo del promedio sea reutilizable y testeable.

## Profundización
Refactorizar código no es un concepto moderno; precede al uso generalizado de computadoras. La práctica probablemente comenzó en el ámbito de la ingeniería mecánica o incluso antes. En el software, se formalizó más con la llegada de la programación orientada a objetos y la programación extrema (XP) en los años 90, notablemente influenciado por el libro seminal de Martin Fowler "Refactoring: Improving the Design of Existing Code."

Existen numerosas técnicas de refactorización, desde simplemente renombrar variables para claridad hasta patrones más complejos como extraer métodos o clases. La clave es hacer pequeños cambios incrementales que no modifiquen la funcionalidad del software pero mejoren la estructura interna.

Al usar Go, la refactorización puede ser directa debido a la simplicidad del lenguaje y su poderosa biblioteca estándar. Sin embargo, sigue siendo importante tener un buen conjunto de pruebas unitarias para asegurar que la refactorización no introduzca errores. Herramientas como `gorename` y `gofmt` ayudan a automatizar algunos de los procesos, y los IDEs a menudo tienen soporte integrado para refactorización.

Además de la refactorización manual, hay algunas herramientas de refactorización de código automatizadas disponibles para Go, como las herramientas de refactorización de GoLand y Go Refactor. Aunque estas pueden acelerar el proceso, no son un sustituto para entender el código y hacer cambios considerados.

## Ver También
 - [Refactorización en Go: Lo Simple es Bello](https://go.dev/blog/slices) 
 - [Go Efectivo: Refactorización con Interfaces](https://go.dev/doc/effective_go#interfaces)
 - [Página de Refactorización de Martin Fowler](https://refactoring.com/)
 - [Herramientas de Refactorización de GoLand](https://www.jetbrains.com/go/features/refactorings/)
