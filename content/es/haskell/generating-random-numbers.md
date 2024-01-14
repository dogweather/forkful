---
title:                "Haskell: Generando números aleatorios"
simple_title:         "Generando números aleatorios"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# ¿Por qué deberías utilizar números aleatorios en Haskell?

La generación de números aleatorios es una habilidad fundamental en la programación, permitiendo la creación de programas y algoritmos más interesantes e impredecibles. Utilizar números aleatorios en Haskell puede llevar tus habilidades de programación al siguiente nivel e introducirte a nuevos conceptos y técnicas.

## Cómo hacerlo

Generar números aleatorios en Haskell es simple gracias a la biblioteca `System.Random`. Primero, debes importar el módulo utilizando `import System.Random`. Luego, puedes utilizar la función `random` para generar un número aleatorio en un rango específico.

```Haskell
import System.Random

-- Generar un número aleatorio entre 1 y 10
randomNum :: IO Int 
randomNum = randomRIO (1, 10)

main :: IO ()
main = do
  num <- randomNum
  putStrLn "Tu número aleatorio es:"
  print num
```

La función `randomRIO` toma dos argumentos: el inicio del rango y el final del rango. En nuestro ejemplo, el número aleatorio estará entre 1 y 10. También puedes utilizar la función `randomIO` para generar un número aleatorio de cualquier tipo, como una lista, un carácter o incluso una función.

```Haskell
import System.Random

-- Generar una lista aleatoria de 5 elementos
randomList :: IO [Int]
randomList = sequence [randomIO | _ <- [1..5]]

main :: IO ()
main = do
  list <- randomList
  putStrLn "Tu lista aleatoria es:"
  print list
```

## Profundizando en la generación de números aleatorios

La función `randomRIO` utiliza un concepto llamado "generador de números aleatorios" para producir números aleatorios. Este generador comienza con una semilla y utiliza un algoritmo para producir una secuencia aparentemente aleatoria de números. La semilla determina la secuencia de números generados, lo que significa que usando la misma semilla obtendremos la misma secuencia de números. Esto es útil para hacer que tus programas sean predecibles en ciertas circunstancias.

Sin embargo, también puedes utilizar una semilla diferente para obtener una secuencia diferente de números aleatorios. En nuestro ejemplo, no especificamos una semilla, por lo que Haskell utiliza una semilla predeterminada, pero puedes proporcionar tu propia semilla utilizando la función `newStdGen`.

```Haskell
import System.Random

-- Generar una lista aleatoria de 5 elementos utilizando una semilla personalizada
randomList :: Int -> IO [Integer]
randomList seed = do
  gen <- newStdGen
  return $ take 5 (randomRs (1, 10) gen)

main :: IO ()
main = do
  list <- randomList 42 -- utilizar la semilla 42
  putStrLn "Tu lista aleatoria es:"
  print list
```

## Ver también

- [Documentación de System.Random en Haskell.org](https://haskell.org/hoogle/?hoogle=System.Random)
- [Tutorial sobre números aleatorios en Haskell](https://www.fpcomplete.com/haskell/tutorial/randoms)