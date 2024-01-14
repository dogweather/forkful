---
title:                "Swift: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una función útil en la programación para crear aplicaciones que requieran aleatoriedad, como juegos o simulaciones.

## Cómo hacerlo

Para generar números aleatorios en Swift, podemos utilizar la función `random()` que nos devuelve un número aleatorio entre 0 y 1. Podemos multiplicar este número por un rango específico para obtener números aleatorios dentro de ese rango. Por ejemplo:

```Swift
let randomNumber = random() * 10 // Devuelve un número aleatorio entre 0 y 10
```

También podemos utilizar la función `arc4random_uniform()` para generar números enteros aleatorios en un rango determinado. Esta función toma como argumento un número entero y genera un número aleatorio entre 0 y ese número menos 1. Por ejemplo:

```Swift
let randomNumber = arc4random_uniform(100) // Devuelve un número aleatorio entre 0 y 99
```

Podemos utilizar un bucle `for` para generar múltiples números aleatorios dentro de un rango, como en el siguiente ejemplo:

```Swift
for _ in 1...10 {
  let randomNumber = arc4random_uniform(10) // Genera 10 números aleatorios entre 0 y 9
  print(randomNumber)
}
```

## Profundizando

Existen diferentes maneras de generar números aleatorios en Swift, dependiendo de nuestras necesidades. Además de las funciones `random()` y `arc4random_uniform()`, también podemos utilizar la estructura `Random` y el protocolo `RandomNumberGenerator` para generar números aleatorios. También podemos utilizar la librería `GameplayKit` para generar números aleatorios con distribuciones específicas.

Es importante tener en cuenta que, aunque los números generados por estas funciones parecen aleatorios, en realidad siguen un patrón determinado. Si necesitamos una verdadera aleatoriedad, debemos utilizar fuentes externas de generación de números aleatorios.

## Ver también

- [The Swift Programming Language: Random Numbers](https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#ID546)
- [Stack Overflow: How to generate a random number in Swift](https://stackoverflow.com/questions/24007129/how-does-one-generate-a-random-number-in-apples-swift-language)