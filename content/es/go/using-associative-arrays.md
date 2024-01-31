---
title:                "Uso de matrices asociativas"
date:                  2024-01-30T19:11:22.432002-07:00
model:                 gpt-4-0125-preview
simple_title:         "Uso de matrices asociativas"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Los arreglos asociativos, conocidos como mapas en Go, te permiten almacenar y acceder a datos con pares clave-valor. Son esenciales para manejar colecciones donde puedes buscar valores rápidamente por una clave única, simplificando la manipulación y recuperación de datos en tus programas.

## Cómo hacerlo:

En Go, los mapas son sencillos de usar. Aquí tienes una guía simple para empezar:

1. **Declarando e Inicializando Mapas**

```Go
package main

import "fmt"

func main() {
    // Inicializa un mapa vacío con claves de tipo string y valores de tipo int
    var scores map[string]int
    fmt.Println(scores) // Imprime: map[]

    // Declarando e inicializando un mapa no vacío
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // Imprime: map[green:#00ff00 red:#ff0000]
}
```

2. **Agregando y Accediendo a Elementos**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // Imprime: 5
}
```

3. **Iterando Sobre Mapas**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s hace %s\n", key, value)
    }
    // El orden de salida puede variar, ya que los mapas no garantizan un orden.
}
```

4. **Eliminando Elementos**

```Go
func main() {
    meals := map[string]int{"desayuno": 300, "almuerzo": 600}
    fmt.Println(meals) // Antes de la eliminación

    delete(meals, "almuerzo")
    fmt.Println(meals) // Después de la eliminación
}
```

## Análisis Detallado

Introducidos en Go 1, los mapas ofrecen una manera incorporada de manejar arreglos asociativos de manera eficiente. A diferencia de las listas, que son colecciones ordenadas, los mapas son desordenados. Esto significa que el orden de iteración sobre los elementos del mapa no está garantizado que sea el mismo en ejecuciones distintas, un compromiso por su capacidad para manejar pares clave-valor de manera dinámica y con significativa flexibilidad.

Por debajo, Go implementa mapas como tablas hash, asegurando que la complejidad promedio de las operaciones de acceso, inserción y eliminación sea O(1), bajo la mayoría de las circunstancias. Sin embargo, vale la pena mencionar que esta eficiencia puede variar basado en factores como colisiones de hash.

Para casos de uso que requieran un recorrido de claves ordenado, podrías considerar combinar mapas con listas o explorar paquetes de terceros que ofrezcan estructuras de datos adicionales como mapas ordenados o árboles. A pesar de sus limitaciones, los mapas de Go son una herramienta poderosa y esencial para muchos escenarios de programación.
