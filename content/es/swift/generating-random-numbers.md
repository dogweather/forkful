---
title:    "Swift: Generación de números aleatorios"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios es una parte fundamental de la programación para crear juegos, aplicaciones de azar, y realizar pruebas de software. También puede ser útil en situaciones donde se desee simular o generar datos de manera aleatoria.

## Cómo hacerlo

Para generar un número aleatorio en Swift, se puede utilizar la función `random()` en conjunto con la clase `Int`. Por ejemplo, si queremos generar un número aleatorio entre 1 y 100, podemos usar el siguiente código:

```
let randomNumber = Int.random(in: 1...100)
print(randomNumber)
```

Este código crea una constante llamada `randomNumber` que contiene un número entero aleatorio entre 1 y 100 y luego lo imprime en la consola. Otra forma de generar un número aleatorio es utilizando la función `arc4random_uniform()`, que devuelve un número entero entre 0 y el número que se le pasa como parámetro menos uno. Por ejemplo:

```
let randomNumber = Int(arc4random_uniform(100))
print(randomNumber)
```

Con este código, se generará un número aleatorio entre 0 y 99 y se imprimirá en la consola.

## Profundizando

La generación de números aleatorios en programación no es en realidad aleatoria, sino que se basa en un algoritmo que calcula una secuencia de números que parecen ser aleatorios. En Swift, se utiliza el algoritmo Mersenne Twister, que es uno de los algoritmos más confiables y ampliamente utilizados para generar números aleatorios.

Además, es importante tener en cuenta que en programación, los números aleatorios se generan a partir de una semilla, que es básicamente un punto de inicio para el algoritmo. Si se utiliza la misma semilla, se generará la misma secuencia de números. Por esta razón, es recomendable utilizar una semilla diferente cada vez que se desee generar una secuencia de números aleatorios.

## Ver también

- [Documentación oficial de Swift sobre generación de números aleatorios](https://developer.apple.com/documentation/swift/double/random)
- [Artículo sobre la importancia de utilizar semillas diferentes en la generación de números aleatorios](https://www.calculatorsoup.com/calculators/math/randomseed.php)
- [Más información sobre el algoritmo Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)