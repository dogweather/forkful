---
title:    "Haskell: Generando números aleatorios"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Haskell

Generar números aleatorios es una habilidad importante en la programación, ya que permite agregar un elemento de incertidumbre o aleatoriedad en nuestros programas. En Haskell, esto puede ser útil para simular situaciones aleatorias o para probar algoritmos. Además, la generación de números aleatorios nos permite crear programas más interesantes y creativos.

## Cómo hacerlo en Haskell

La primera cosa que necesitamos hacer es importar el módulo `System.Random`, el cual nos permite generar números aleatorios en Haskell. Luego, podemos utilizar la función `randomR` para generar un número aleatorio dentro de un rango específico. Por ejemplo, si queremos generar un número entre 1 y 10, podemos escribir el siguiente código:

```Haskell
import System.Random

numeroAleatorio = randomR (1, 10)
```

La función `randomR` devuelve un objeto `IO` que debemos convertir a un entero utilizando la función `random`. Entonces, nuestro código quedaría así:

```Haskell
import System.Random

numeroAleatorio = do
    r <- randomR (1, 10)
    return (random r)
```

Si corremos este código varias veces, veremos que se generan números distintos cada vez.

## Profundizando en la generación de números aleatorios

Haskell utiliza un generador de números aleatorios llamado "Mersenne Twister". Este generador produce secuencias de números de alta calidad y tiene una gran velocidad de generación. Además, Haskell utiliza un enfoque perezoso (lazy) en la generación de números aleatorios, lo que significa que no se generan todos los números en el momento en que se llama a la función `randomR`. En su lugar, se generan sólo los suficientes para satisfacer la necesidad actual.

Otra función útil para generar aleatoriedad es `randomIO`, la cual genera un número aleatorio aleatorio dentro de todo el rango posible de un tipo específico. Por ejemplo, si queremos generar un booleano aleatorio, podemos utilizar esta función de la siguiente manera:

```Haskell
import System.Random

boolAleatorio = randomIO :: IO Bool
```

### Ver también

- [Documentación oficial del módulo `System.Random`](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Artículo sobre generación de números aleatorios en Haskell](https://wiki.haskell.org/Random_numbers)