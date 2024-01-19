---
title:                "Generando números aleatorios"
html_title:           "Arduino: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Generar números aleatorios es el proceso de producir secuencias que no pueden ser predichas de manera lógica. Los programadores lo hacen para simular eventos aleatorios, como tirar un dado en un juego.

## ¿Cómo se hace?

Haskell tiene una biblioteca estándar llamada `System.Random` que podemos usar para generar números aleatorios. Aquí está un ejemplo básico:

```Haskell
import System.Random

generarAleatorio :: IO Int
generarAleatorio = getStdRandom (randomR (1, 100))

main = do
    numero <- generarAleatorio
    print numero
```

Este programa genera un número aleatorio entre 1 y 100. Cada vez que lo ejecutes, verás un número diferente.

## Conocimiento profundo

Haskell usa un generador de números pseudoaleatorios, el cual genera secuencias de números que parecen aleatorios pero que se repiten tras un cierto periodo. Históricamente, el concepto de aleatoriedad ha sido crucial en la criptografía. En cuanto a alternativas, se podría usar cualquier número de módulos de terceros para generar números aleatorios, pero `System.Random` es suficientemente robusto para la mayoría de las aplicaciones.

En cuanto a los detalles de implementación, `randomR` genera un número aleatorio en el rango especificado, y `getStdRandom` usa el generador aleatorio global en su ejecución.

## Consultar también

1. [Haskell: Biblioteca System.Random](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html)

2. [Introducción a `System.Random` en Haskell](http://book.realworldhaskell.org/read/using-typeclasses.html#id689872)

3. [Guía básica de generación de números aleatorios en Haskell](https://wiki.haskell.org/Random_numbers)

Estos recursos pueden proporcionarte más detalles sobre cómo trabajar con números aleatorios en Haskell.