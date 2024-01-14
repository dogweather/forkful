---
title:    "Go: Generando números aleatorios"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Go?

Generar números aleatorios es una función esencial en muchas aplicaciones y juegos. En Go, podemos utilizar esta funcionalidad para crear pruebas aleatorias, seleccionar elementos al azar de una lista o incluso crear juegos de azar. Aprender cómo generar números aleatorios en Go puede ser útil en una amplia variedad de proyectos.

## Cómo hacerlo

Para generar números aleatorios en Go, primero debemos importar el paquete "math/rand". Luego, utilizaremos la función "rand.Intn()" para generar un número aleatorio entre 0 y el número especificado (no incluido). Por ejemplo, si queremos generar un número aleatorio entre 1 y 10, utilizaremos:

```Go
rand.Intn(10) + 1
```

También podemos generar números aleatorios de punto flotante utilizando la función "rand.Float64()". Esta función generará un número aleatorio entre 0.0 (incluido) y 1.0 (no incluido). Podemos especificar un rango personalizado multiplicando el resultado por un número y sumando un valor. Por ejemplo, si queremos generar un número aleatorio entre 0.0 y 10.0, utilizaremos:

```Go
rand.Float64() * 10.0
```

Aquí hay un ejemplo completo de cómo generar 10 números aleatorios entre 1 y 100:

```Go
package main

import (
    "fmt"
    "math/rand"
)

func main() {
    for i := 0; i < 10; i++ {
        fmt.Println(rand.Intn(100) + 1)
    }
}
```

Este código imprimirá 10 números aleatorios entre 1 y 100. Ejemplo de salida:

```
67
29
83
4
96
51
14
99
6
52
```

## Un análisis más profundo

En Go, los números aleatorios generados por la función "rand.Intn()" son pseudoaleatorios, lo que significa que no son completamente aleatorios y siguen un patrón predecible. El generador de números aleatorios se inicializa con una semilla (seed) que determina la secuencia de números que se generarán. La semilla por defecto es 1, pero podemos especificar una semilla personalizada utilizando la función "rand.Seed()".

Es importante tener en cuenta que, por defecto, el generador de números aleatorios en Go no es seguro para concurrencia. Esto significa que no se debe utilizar en aplicaciones concurrentes sin tomar las precauciones adecuadas. Para obtener más información sobre cómo utilizar números aleatorios de manera segura en aplicaciones concurrentes, consulte la documentación de Go.

## Ver también

- Documentación de Go sobre el paquete "math/rand": https://golang.org/pkg/math/rand/
- Documentación de Go sobre cómo utilizar números aleatorios de manera segura en aplicaciones concurrentes: https://golang.org/pkg/math/rand/#Intro