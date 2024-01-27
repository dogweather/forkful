---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir tests es crear casos de prueba automáticos para asegurarse de que tu código funciona como esperas. Lo hacemos para cazar bugs antes de que lleguen a producción, ahorrar tiempo en pruebas manuales y mantener la calidad del código a largo plazo.

## Cómo se hace:
```Haskell
-- Usamos Hspec, un framework de testing para Haskell.
-- Primero, importamos el módulo.
import Test.Hspec

-- Definimos una función simple a testear.
sumar :: Int -> Int -> Int
sumar x y = x + y

-- Escribimos nuestros tests.
main :: IO ()
main = hspec $ do
  describe "sumar" $ do
    it "sumar 1 y 2 resulta en 3" $ do
      sumar 1 2 `shouldBe` 3

    it "sumar 0 y 5 resulta en 5" $ do
      sumar 0 5 `shouldBe` 5
```

Output:
```
sumar
  sumar 1 y 2 resulta en 3
  sumar 0 y 5 resulta en 5

Finished in 0.0001 seconds
2 examples, 0 failures
```

## Profundización
El testing en Haskell tiene sus raíces en la cultura de la programación funcional, con un enfoque en funciones puras que son ideales para testing. Alternativas como QuickCheck permiten tests basados en propiedades donde se generan entradas aleatorias. La implementación de tests hace uso de funciones de alto orden, lo cual se alinea con el paradigma funcional de Haskell.

## Ver También
- [Hspec documentation](https://hspec.github.io/)
- [QuickCheck on Hackage](https://hackage.haskell.org/package/QuickCheck)
