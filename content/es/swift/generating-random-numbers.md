---
title:                "Generando números aleatorios"
html_title:           "Swift: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Generar números aleatorios es una técnica utilizada por los programadores para obtener valores aleatorios en sus programas. Esto puede ser útil en situaciones como juegos, sorteos o para probar diferentes escenarios en un programa.

## Cómo hacerlo:

Puedes generar números aleatorios en Swift utilizando la función `random()`, que devuelve un número aleatorio entre 0 y 1. Por ejemplo:

```Swift
let randomNumber = random()
print(randomNumber)

// Output: 0.781243432
```

También puedes especificar un rango y obtener un número aleatorio dentro de ese rango utilizando la función `random(in:)`. Por ejemplo:

```Swift
let randomNumberInRange = random(in: 1...10)
print(randomNumberInRange)

// Output: 7
```

## Profundizando:

La generación de números aleatorios tiene una larga historia en la informática y ha sido utilizada en una variedad de algoritmos y aplicaciones. Otras formas de generar números aleatorios incluyen utilizar la hora actual como semilla, algoritmos basados en cifrado y generadores de números pseudoaleatorios.

También es importante tener en cuenta que los números generados aleatoriamente no son verdaderamente aleatorios, sino que se denominan números pseudoaleatorios. Esto significa que siguen un patrón predecible y pueden repetirse si se utiliza la misma semilla.

## Ver también:

Puedes obtener más información sobre la generación de números aleatorios en Swift en la documentación oficial de Apple: https://developer.apple.com/documentation/swift/random. También puedes explorar diferentes métodos para generar números pseudoaleatorios en la documentación de la API de Swift: https://developer.apple.com/documentation/swift/randomnumbergenerator.