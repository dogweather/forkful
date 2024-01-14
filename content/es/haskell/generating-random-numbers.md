---
title:    "Haskell: Generación de números aleatorios"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## ¿Por qué generar números aleatorios en Haskell?

Generar números aleatorios es una función importante en la programación. Puede ser utilizado para simular situaciones aleatorias en juegos, probar algoritmos y modelos matemáticos, y muchas otras aplicaciones. En Haskell, hay varias maneras de generar números aleatorios, lo cual lo convierte en un lenguaje muy versátil para este propósito.

## Como hacerlo en Haskell

Generar números aleatorios en Haskell es sorprendentemente sencillo gracias a la librería `random` incluida en el lenguaje. Para usar esta librería, primero debemos importarla con `import System.Random`.

Existen varias funciones en la librería `random` que nos permiten generar diferentes tipos de números aleatorios. Por ejemplo, la función `randomR` nos permite generar un número aleatorio en un rango específico. Veamos un ejemplo de cómo generar un número aleatorio entre 1 y 10 en Haskell:

```Haskell
import System.Random

randomNumber :: IO Int
randomNumber = do
  gen <- getStdGen
  let (rand, newGen) = randomR (1, 10) gen
  setStdGen newGen
  return rand
```

En este código, primero importamos la librería `System.Random` y luego definimos una función llamada `randomNumber` que utiliza la función `randomR` para generar un número entero aleatorio en el rango especificado. Para esto, utilizamos la función `getStdGen` para obtener un generador de números aleatorios y luego utilizamos la función `randomR` para generar un número en el rango especificado. Luego, utilizamos la función `setStdGen` para actualizar nuestro generador de números aleatorios y asegurarnos de que cada vez que llamemos a `randomNumber` obtengamos un número diferente.

Otra función útil en la librería `random` es `random`, la cual nos permite generar cualquier tipo de dato aleatorio. Por ejemplo, podemos generar un número aleatorio de tipo `Double` con la siguiente función:

```Haskell
import System.Random

randomDouble :: IO Double
randomDouble = do
  gen <- getStdGen
  let (rand, newGen) = random gen
  setStdGen newGen
  return rand
```

## Profundizando en la generación de números aleatorios en Haskell

La librería `random` en Haskell utiliza el concepto de Generators para generar números aleatorios. Un Generador es básicamente una función que toma una semilla y devuelve un número aleatorio y un nuevo Generador. Este proceso de tomar una semilla y generar un nuevo número aleatorio se conoce como "splatting".

Hay varias funciones en la librería `random` que utilizan este concepto de Generators, como `randomR` y `random`. También podemos crear nuestro propio Generador utilizando la función `mkStdGen`. Por ejemplo, en lugar de utilizar `getStdGen` y `setStdGen` como en los ejemplos anteriores, podemos crear un Generador personalizado y pasarlo como argumento a nuestra función `randomR`:

```Haskell
import System.Random

randomNumber' :: StdGen -> Int
randomNumber' gen = let (rand, _) = randomR (1, 10) gen
                    in rand
```

Este código es equivalente al primer ejemplo, pero utilizamos nuestro Generador personalizado en lugar de `getStdGen` y `setStdGen`.

En resumen, generación de números aleatorios en Haskell es una tarea simple gracias a la librería `random` y el concepto de Generators. Ahora puedes utilizar esta habilidad en tus proyectos o para explorar diferentes algoritmos y modelos matemáticos.

## Ver también

- [Documentación oficial de la librería `random` en Haskell](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Tutorial de Haskell en español](https://haskelles.com/)
- [Ejemplos de programación en Haskell en GitHub](https://github.com/vrom911/haskell-learning)