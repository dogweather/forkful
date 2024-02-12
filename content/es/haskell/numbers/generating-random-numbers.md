---
title:                "Generación de números aleatorios"
aliases:
- es/haskell/generating-random-numbers.md
date:                  2024-01-27T20:33:48.512838-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generación de números aleatorios"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Generar números aleatorios en Haskell implica crear números que son impredecibles según los estándares humanos. Esto es crítico en escenarios que van desde aplicaciones criptográficas hasta simulaciones donde el elemento de azar es requerido para modelar fenómenos del mundo real de manera precisa.

## Cómo hacerlo:

Para generar números aleatorios en Haskell, uno típicamente usa el paquete `random`, que es parte de la Plataforma Haskell. Aquí tienes una guía paso a paso:

Primero, asegúrate de tener instalado el paquete `random`. Si no, puedes obtenerlo vía Cabal o Stack.

### Generando un Número Aleatorio

Para generar un número aleatorio simple, puedes usar la función `randomRIO`, la cual produce un valor aleatorio dentro de un rango especificado.

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Número aleatorio: " ++ show randomNumber
```

### Generando una Lista de Números Aleatorios

Generar una lista de números aleatorios es un poco más complicado pero aún así directo:

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n-1)
  return (r:rs)

main :: IO ()
main = do
  numbers <- randomList 5
  print numbers
```

Este fragmento de código crea una función `randomList` que genera una lista de enteros aleatorios. Reemplaza `(1, 100)` con tu rango deseado.

## Profundización

El paquete `random` de Haskell proporciona un generador de números pseudoaleatorios (PRNG), lo que significa que los números generados no son verdaderamente aleatorios pero pueden parecerlo para muchas aplicaciones. El núcleo de la capacidad de generación aleatoria de Haskell reside en la clase de tipo `RandomGen`, que abstrae diferentes métodos de generar números aleatorios, y la clase de tipo `Random`, que incluye tipos que pueden ser generados aleatoriamente.

Históricamente, el enfoque de Haskell para la generación de números aleatorios ha enfatizado la pureza y la reproducibilidad. Es por esto que las operaciones que involucran aleatoriedad se manejan explícitamente en el monad `IO` o requieren pasar y actualizar manualmente los estados del generador — para mantener la transparencia referencial.

En ciertas aplicaciones, como la criptografía, los números pseudoaleatorios generados por el PRNG predeterminado pueden no ser lo suficientemente seguros. Para estos casos de uso, los programadores de Haskell a menudo recurren a bibliotecas más especializadas como `crypto-random`, que están diseñadas para cumplir con los requisitos estrictos de las aplicaciones criptográficas.

Además, bibliotecas alternativas como `mwc-random` ofrecen un mejor rendimiento y calidad de los números aleatorios para simulaciones y otras aplicaciones, implementando algoritmos modernos como el Mersenne Twister.

Al elegir un enfoque de generación de números aleatorios en Haskell, es esencial considerar las necesidades de la aplicación respecto a la calidad del azar, rendimiento y seguridad para seleccionar la herramienta o biblioteca más apropiada.
