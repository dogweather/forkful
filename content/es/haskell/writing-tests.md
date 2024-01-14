---
title:    "Haskell: Escribiendo pruebas"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-tests.md"
---

{{< edit_this_page >}}

#¿Por qué deberías escribir pruebas en tus programas Haskell?

Escribir pruebas en tus programas Haskell puede ser un proceso tedioso y puede parecer una tarea innecesaria, pero en realidad es una práctica muy importante en el desarrollo de software. Las pruebas nos ayudan a detectar errores en nuestro código y garantizar que nuestras funciones y módulos funcionen correctamente. Además, nos permite tener una mejor comprensión de nuestro código y facilita su mantenimiento en el futuro.

##Cómo escribir pruebas en Haskell

Para escribir pruebas en Haskell, primero debemos importar el módulo `Test.HUnit` en nuestro archivo de código. Este módulo nos proporciona las herramientas necesarias para escribir y ejecutar pruebas unitarias.

Para crear una prueba, podemos utilizar la función `TestLabel` que nos permite nombrar nuestra prueba y la función `TestCase` para escribir la lógica de nuestra prueba. Por ejemplo:

```Haskell
import Test.HUnit

-- Definimos una función para sumar dos números
sumar :: Int -> Int -> Int
sumar x y = x + y

-- Creamos nuestra prueba
pruebaSuma :: Test
pruebaSuma = TestLabel "Prueba de suma" $
  TestCase $ assertEqual "Suma incorrecta" 35 (sumar 15 20)
```

En este ejemplo, estamos probando la función `sumar` para asegurarnos de que suma correctamente dos números enteros. Utilizamos la función `assertEqual` para comparar el resultado esperado con el resultado real de nuestra función.

Podemos ejecutar nuestras pruebas utilizando la función `runTestTT` y pasando nuestro conjunto de pruebas como argumento. Por ejemplo:

```Haskell
-- Ejecutamos nuestras pruebas
main :: IO Counts
main = runTestTT $ TestList [pruebaSuma]
```

Si ejecutamos nuestro programa, deberíamos obtener la siguiente salida:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 0
```

Si cambiamos alguno de los valores en nuestra prueba para que falle, por ejemplo `sumar 15 25`, obtendríamos la siguiente salida:

```
Cases: 1  Tried: 1  Errors: 0  Failures: 1
Failure:
Suma incorrecta
expected: 35
 but got: 40
```

##Profundizando en la escritura de pruebas

Ahora que ya sabemos cómo escribir pruebas en Haskell, es importante profundizar y aprender más sobre esta práctica. Hay diferentes tipos de pruebas que podemos escribir en Haskell, como pruebas unitarias, pruebas de integración y pruebas de propiedades.

También es importante aprender sobre técnicas de prueba como la cobertura de código y la generación de casos de prueba aleatorios para asegurarnos de que estamos probando todos los casos posibles y encontrando cualquier posible error en nuestro código.

La escritura de pruebas también puede ser una gran herramienta para el aprendizaje y la experimentación en Haskell. Al escribir pruebas, podemos explorar diferentes conceptos y funciones y asegurarnos de entenderlos correctamente. Además, al tener pruebas en nuestro código, podemos realizar cambios y refactorizaciones sin temor a introducir nuevos errores.

#Ver también

- [HUnit Tutorial en HaskellWiki](https://wiki.haskell.org/HUnit_Tutorial)
- [Refactorizando con pruebas en Haskell por Kowainik](https://kowainik.github.io/posts/refactor-testing/)
- [Hspec - Biblioteca para escribir pruebas en Haskell](https://hspec.github.io/)