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

## Por qué

Generar números aleatorios es una habilidad importante en la programación ya que permite crear aplicaciones más dinámicas y divertidas. Además, puede ser útil en casos como la generación de contraseñas seguras o la selección al azar de elementos en una lista.

## Cómo hacerlo

Para generar números aleatorios en Swift, podemos utilizar la función `arc4random_uniform` que nos devuelve un número aleatorio dentro de un rango determinado. Por ejemplo:

```Swift
let randomNumber = arc4random_uniform(10)
// Esto nos devuelve un número aleatorio entre 0 y 9
```

También podemos utilizar la función `Int.random(in:)` que nos permite especificar un rango personalizado. Por ejemplo:

```Swift
let randomNumber = Int.random(in: 1...100)
// Esto nos devuelve un número aleatorio entre 1 y 100
```

Para obtener un número aleatorio de tipo `Double`, podemos utilizar la función `Double.random(in:)`. Por ejemplo:

```Swift
let randomNumber = Double.random(in: 0..<1)
// Esto nos devuelve un número aleatorio entre 0 y 1
```

## Profundizando

La función `arc4random_uniform` utiliza el generador de números aleatorios Xorshift de 128 bits. Este generador se caracteriza por ser rápido y producir resultados de alta calidad, pero no es adecuado para aplicaciones criptográficas debido a su predecibilidad. Por lo tanto, si se necesita una mayor seguridad en los números generados, se recomienda utilizar la clase `Random` de la biblioteca `CryptoKit`.

Otro aspecto importante a tener en cuenta al generar números aleatorios es la semilla o "seed" utilizada. La semilla es un número que se utiliza para iniciar el generador de números aleatorios y determina la secuencia de valores que se generarán. Si no se especifica una semilla, la función `arc4random_uniform` utilizará como semilla el reloj del sistema, lo que significa que si se llama varias veces en poco tiempo, se obtendrán números muy similares.

## Ver también

- Documentación oficial de Apple sobre la función `arc4random_uniform`: https://developer.apple.com/documentation/swift/arc4random_uniform
- Documentación oficial de Apple sobre la clase `Random` en la biblioteca `CryptoKit`: https://developer.apple.com/documentation/cryptokit/random