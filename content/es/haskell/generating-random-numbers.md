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

# ¿Qué y por qué?

Generar números aleatorios es un proceso en el que se crean números al azar para su uso en programas informáticos. Los programadores a menudo usan esta función para crear variables aleatorias en sus programas, lo que puede ser útil en juegos, simulaciones y otras aplicaciones.

# Cómo hacerlo:

```Haskell
import System.Random

-- Generar un número aleatorio entre 1 y 10
randomNum :: IO Int
randomNum = getStdRandom (randomR (1, 10))

-- Imprimir el número generado
main :: IO ()
main = do
  num <- randomNum
  print num
```

Salida de muestra:
```Haskell
7
```

# Profundizando:

1. Contexto histórico:
La generación de números aleatorios ha sido ampliamente utilizada en la industria de los juegos de azar desde la década de 1940. En la década de 1960, se desarrollaron los primeros algoritmos para generar números aleatorios en computadoras.

2. Alternativas:
Además de la función de Haskell `randomR`, existen otras formas de generar números aleatorios, como `randomIO` y `randomRs`.

3. Detalles de implementación:
La función `randomR` toma una semilla de número aleatorio y devuelve un número dentro del rango especificado. La semilla se puede generar a partir de funciones como `getStdRandom` o `newStdGen` en el módulo `System.Random`.

# Ver también:

- Documentación de Haskell sobre generación de números aleatorios: https://www.haskell.org/documentation/
- Ejemplos de implementación de generación de números aleatorios en Haskell: https://github.com/search?q=random+number+generation+haskell&type=Repositories