---
title:                "Haskell: Escribiendo pruebas"
programming_language: "Haskell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

La escritura de pruebas es una práctica importante en cualquier lenguaje de programación, incluyendo Haskell. Al escribir pruebas, podemos tener una mayor confianza en nuestro código y asegurarnos de que funcione como lo esperado. Además, las pruebas nos ayudan a detectar errores y problemas en nuestro código antes de que lleguen a producción.

## Cómo hacerlo

Para escribir pruebas en Haskell, utilizamos el módulo `Test.HUnit`, que proporciona funciones para crear y ejecutar pruebas unitarias. Primero, importamos el módulo en nuestro archivo de código:

```Haskell
import Test.HUnit
```

Luego, podemos definir nuestras pruebas de la siguiente manera:

```Haskell
test1 = TestCase (assertEqual "1 + 1 is equal to 2" 2 (1+1))

test2 = TestCase (assertEqual "3 * 5 is equal to 15" 15 (3*5))

tests = TestList [TestLabel "Test 1" test1, TestLabel "Test 2" test2]
```

En estas pruebas, estamos verificando si nuestras operaciones matemáticas básicas son correctas. Para ejecutar las pruebas, podemos utilizar la función `runTestTT` y pasarle `tests` como argumento:

```Haskell
main = runTestTT tests
```

Esto generará una salida de prueba que nos informará si nuestras pruebas fueron exitosas o si se encontraron errores.

## Profundizando

Al escribir pruebas en Haskell, es importante tener en cuenta que nuestro código debe ser lo más puro posible, es decir, sin efectos secundarios. Esto puede ser un desafío al principio, pero nos obliga a escribir código más modular y fácil de probar.

También es importante recordar que las pruebas deben ser específicas y cubrir todos los casos posibles. De esta manera, podemos estar seguros de que nuestro código funciona no solo en un escenario ideal, sino también en situaciones más complejas.

## Ver también

- [HUnit Tutorial](https://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit-Tutorial.html)
- [Pruebas unitarias en Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple-Testing-in-Haskell)
- [HUnitTest Documentación](https://hackage.haskell.org/package/HUnit-1.6.0.0/docs/Test-HUnit.html)