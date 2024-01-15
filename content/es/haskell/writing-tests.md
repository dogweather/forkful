---
title:                "Escribiendo pruebas"
html_title:           "Haskell: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Haskell?

Escribir pruebas es una parte esencial del proceso de desarrollo de software. Además de proporcionar seguridad y confiabilidad a nuestro código, también nos permite detectar errores y corregirlos antes de que se conviertan en problemas mayores. En Haskell, escribir pruebas es especialmente importante ya que su sistema de tipos fuertemente tipado nos ayuda a prevenir errores de forma más eficaz.

## Cómo hacerlo

La forma más común de escribir pruebas en Haskell es utilizando la biblioteca de pruebas "HUnit". Esta biblioteca proporciona una sintaxis sencilla para definir y ejecutar pruebas. Por ejemplo, si queremos probar una función que suma dos números:

```Haskell
-- Importamos la biblioteca HUnit
import Test.HUnit

-- Definimos nuestra función
suma :: Int -> Int -> Int
suma x y = x + y

-- Escribimos una prueba utilizando la sintaxis de HUnit
testSuma = TestCase $ assertEqual "Suma incorrecta" 5 (suma 2 3)

-- Ejecutamos las pruebas
ejecutarPruebas = runTestTT $ TestList [testSuma]

-- Deberíamos recibir el siguiente resultado:
-- ##### ### Label: Suma incorrecta
-- ### Cases: 1  Tried: 1  Errors: 0  Failures: 0
-- ### Test Result: Pass
```

En este ejemplo, utilizamos la función `TestCase` para definir una prueba y la función `assertEqual` para verificar que el resultado de la función `suma` sea igual a 5 cuando se le pase 2 y 3 como argumentos. Luego, ejecutamos la prueba utilizando la función `runTestTT` y especificando la prueba que queremos ejecutar. Al ejecutar las pruebas, deberíamos recibir un resultado que indica que la prueba fue exitosa.

## Profundizando más

Hay muchas otras bibliotecas de pruebas disponibles para Haskell, como QuickCheck, que utiliza pruebas basadas en propiedades, o Tasty, una biblioteca de pruebas con más opciones de personalización y extensibilidad. Además, la comunidad de Haskell es muy activa y siempre está compartiendo recursos y buenas prácticas para escribir pruebas eficaces.

## Ver también

- [Documentación de HUnit](https://hackage.haskell.org/package/HUnit)
- [Introducción a QuickCheck](https://wiki.haskell.org/Introduction_to_QuickCheck)
- [Tutorial de Tasty](https://github.com/feuerbach/tasty/blob/master/doc/Tasty-Tutorial.md)