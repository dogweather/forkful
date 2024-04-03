---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:39.361456-07:00
description: "Los arreglos asociativos, conocidos como mapas en Go, te permiten almacenar\
  \ pares clave-valor donde cada clave \xFAnica se asocia a un valor. Los\u2026"
lastmod: '2024-03-13T22:44:58.459375-06:00'
model: gpt-4-0125-preview
summary: "Los arreglos asociativos, conocidos como mapas en Go, te permiten almacenar\
  \ pares clave-valor donde cada clave \xFAnica se asocia a un valor."
title: Usando arrays asociativos
weight: 15
---

## Qué y Por Qué?

Los arreglos asociativos, conocidos como mapas en Go, te permiten almacenar pares clave-valor donde cada clave única se asocia a un valor. Los programadores utilizan mapas para la recuperación eficiente de datos, la modificación y para mantener una colección de elementos que pueden ser accedidos rápidamente usando claves únicas.

## Cómo hacerlo:

Crear e inicializar un mapa en Go se puede hacer de varias maneras. Aquí tienes un ejemplo básico para empezar:

```go
package main

import "fmt"

func main() {
    // Declarando e inicializando un mapa
    colors := map[string]string{
        "rojo":    "#FF0000",
        "verde":   "#00FF00",
        "azul":    "#0000FF",
    }

    fmt.Println(colors)
    // Salida: map[azul:#0000FF verde:#00FF00 rojo:#FF0000]
}
```

Para agregar o actualizar elementos, asignas un valor a una clave así:

```go
colors["blanco"] = "#FFFFFF"
fmt.Println(colors)
// Salida: map[azul:#0000FF verde:#00FF00 rojo:#FF0000 blanco:#FFFFFF]
```

Acceder a un valor por su clave es sencillo:

```go
fmt.Println("El código hex para el rojo es:", colors["rojo"])
// Salida: El código hex para el rojo es: #FF0000
```

Para eliminar un elemento, usa la función `delete`:

```go
delete(colors, "rojo")
fmt.Println(colors)
// Salida: map[azul:#0000FF verde:#00FF00 blanco:#FFFFFF]
```

Iterar sobre un mapa se hace usando un bucle for:

```go
for color, hex := range colors {
    fmt.Printf("Clave: %s Valor: %s\n", color, hex)
}
```

Recuerda, los mapas en Go no están ordenados. El orden de iteración no está garantizado.

## Estudio Detallado

En Go, los mapas se implementan como tablas hash. Cada entrada en el mapa consiste en dos elementos: una clave y un valor. La clave se hashea para almacenar la entrada, lo que permite operaciones de tiempo constante para un pequeño conjunto de datos y una complejidad de tiempo promedio de O(1) con un hashing adecuado, que puede degradarse a O(n) en el peor caso con muchas colisiones de hash.

Una nota importante para los nuevos programadores de Go es que los tipos de mapa son tipos de referencia. Esto significa que cuando pasas un mapa a una función, cualquier cambio realizado en el mapa dentro de esa función es visible para el llamador. Esto es diferente de, digamos, pasar una estructura a una función, donde la estructura se copia a menos que se pase por un puntero.

Mientras que los mapas son increíblemente versátiles y eficientes para la mayoría de los casos de uso que involucran arreglos asociativos, en aplicaciones críticas para el rendimiento, puede ser beneficioso usar estructuras de datos con características de rendimiento más predecibles, especialmente si las distribuciones de claves pueden causar colisiones frecuentes.

Otra alternativa a considerar es el `sync.Map`, disponible desde Go 1.9, diseñado para casos de uso donde las claves se escriben una sola vez pero se leen muchas veces, ofreciendo mejoras de eficiencia en estos escenarios. Sin embargo, para aplicaciones de Go convencionales, el uso regular de mapas es idiomático y a menudo el enfoque recomendado por su simplicidad y soporte directo en el lenguaje.
