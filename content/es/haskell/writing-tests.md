---
title:                "Pruebas de escritura"
html_title:           "Haskell: Pruebas de escritura"
simple_title:         "Pruebas de escritura"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir pruebas o tests es una práctica común en la programación. Consiste en crear programas que comprueben si nuestro código funciona como esperamos. Los programadores escriben pruebas para asegurar la calidad del software y detectar posibles errores.

## Cómo:
En Haskell, podemos usar el módulo ```Test.HUnit``` para escribir pruebas unitarias. Veamos un ejemplo sencillo de una función que suma dos números y su correspondiente prueba:

```Haskell
sumar :: Int -> Int -> Int
sumar x y = x + y

-- Prueba
TestLabel "Suma" $ TestCase (assertEqual "2 + 3 = 5" 5 (sumar 2 3))
```

La salida de esta prueba debería ser:

```
Cases: 1 Tried: 1 Errors: 0 Failures: 0
```

## Profundizando:
La idea de escribir pruebas no es algo nuevo. En los años 50, se popularizó la práctica de escribir programas para probar otros programas. Existen también otras herramientas en Haskell, como ```QuickCheck```, que generan casos de prueba automáticamente.

Se pueden encontrar alternativas a ```Test.HUnit```, como ```HSpec```, que ofrece una sintaxis más expresiva para escribir pruebas.

Cuando escribimos pruebas, es importante seguir buenas prácticas, como tener pruebas independientes y evitar pruebas redundantes.

## Ver también:
- [Tutorial de tests en Haskell](https://wiki.haskell.org/Testing)
- [Documentación de Test.HUnit](https://hackage.haskell.org/package/HUnit)
- [Documentación de QuickCheck](https://hackage.haskell.org/package/QuickCheck)
- [Documentación de HSpec](https://hackage.haskell.org/package/hspec)