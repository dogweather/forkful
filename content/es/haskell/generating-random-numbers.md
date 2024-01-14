---
title:                "Haskell: Generando números aleatorios"
programming_language: "Haskell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por Qué

Generar números aleatorios es una herramienta esencial en la programación, ya que permite simular diferentes escenarios y tomar decisiones de forma impredecible. Además, también es muy útil en el desarrollo de juegos y aplicaciones de entretenimiento.

## Cómo Hacerlo

En Haskell, el módulo `System.Random` nos proporciona funciones para generar números aleatorios. Primero, debemos importarlo en nuestro código:

```Haskell
import System.Random
```

Luego, podemos utilizar la función `randomR` para generar un número aleatorio dentro de un rango específico. Imprimimos el resultado en la consola utilizando `print`:

```Haskell
print (randomR (1, 10) (mkStdGen 5))
-- Output: (7, StdGen 2984780939 1595832848)
```

En este ejemplo, estamos generando un número aleatorio entre 1 y 10 utilizando una semilla de 5. La semilla es importante porque permite reproducir el mismo número aleatorio en diferentes ejecuciones de nuestro código.

## Profundizando

Para comprender mejor cómo se generan los números aleatorios, es necesario entender el concepto de generador de números aleatorios (RNG, por sus siglas en inglés). Un RNG es un algoritmo que produce una secuencia de números numéricamente aleatorios a partir de una semilla. En Haskell, esto se logra mediante la función `mkStdGen`, que toma una semilla como parámetro y devuelve un `StdGen`, que es el tipo de datos utilizado para representar el generador.

Es importante tener en cuenta que los números aleatorios generados por una semilla determinada siempre serán los mismos, por lo que es común utilizar la función `random` para generar una semilla aleatoria cada vez que se ejecuta el programa.

Otra función útil en el módulo `System.Random` es `randoms`, que devuelve una lista infinita de números aleatorios. Podemos utilizar `take` para obtener un número limitado de elementos de esa lista:

```Haskell
take 5 (randoms (mkStdGen 15) :: [Int])
-- Output: [-614952347,232835004,712836212,819214926,-149058433]
```

También es posible generar números aleatorios de otros tipos de datos, como `Double` o `Bool`. Y si queremos generar números aleatorios en un rango específico pero con una distribución no uniforme, podemos utilizar la función `randomRIO` que utiliza una fuente de entropía externa para producir números aleatorios.

## Ver También

- [Documentación del módulo `System.Random`](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [Introducción a la programación en Haskell (en español)](https://www.haskell.org/tutorial/index.es.html)
- [Tutorial de Random en Haskell (en inglés)](https://wiki.haskell.org/Random_number_generation)