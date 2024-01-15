---
title:                "Generando números aleatorios"
html_title:           "Haskell: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué

Generar números aleatorios puede ser útil en muchos escenarios, desde juegos hasta simulaciones y pruebas de software. Con Haskell, podemos hacerlo de forma sencilla y precisa utilizando la librería `System.Random`.

## Cómo hacerlo

Para generar un número aleatorio en Haskell, debemos seguir los siguientes pasos:

1. Importar la librería `System.Random` en nuestro archivo de código:
```Haskell
import System.Random
```
2. Utilizar la función `random` para generar un número aleatorio en el rango especificado. Por ejemplo, si queremos generar un número entre 1 y 10, podemos hacer lo siguiente:
```Haskell
randomRIO (1, 10)
```
Esta función también puede usarse para otros tipos de datos, como caracteres y elementos de una lista.
3. Para obtener el número aleatorio generado, debemos encerrar esta función en la mónada `IO` y utilizar la función `readLn` para leer la entrada del usuario. Por ejemplo:
```Haskell
numero <- randomRIO (1, 10) :: IO Int
entrada <- readLn
```
La variable `numero` contendrá el número aleatorio generado, mientras que la variable `entrada` contendrá la entrada del usuario.
4. Si queremos generar más de un número aleatorio, podemos utilizar la función `replicate` para repetir el proceso. Por ejemplo, para generar 5 números aleatorios entre 1 y 10:
```Haskell
numeros <- replicate 5 (randomRIO (1, 10) :: IO Int)
```
La variable `numeros` contendrá una lista con los 5 números aleatorios generados.

## Profundizando

Haskell utiliza el concepto de mónadas para trabajar con entradas y salidas, como en el caso de la función `randomRIO`. Además, la librería `System.Random` también incluye otras funciones útiles para trabajar con números aleatorios, como `randomRs` para generar una lista de números aleatorios en un rango especificado y `mkStdGen` para generar una semilla aleatoria.

## Ver también

- [Documentación de la librería `System.Random`](https://hackage.haskell.org/package/random/docs/System-Random.html)
- [Tutorial de Haskell en español](https://haskell-es.gitlab.io/tutorial/)