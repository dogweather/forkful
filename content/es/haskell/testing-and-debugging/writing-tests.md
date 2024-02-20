---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:30:32.052437-07:00
description: "Escribir pruebas en Haskell se trata de asegurar que tus funciones funcionen\
  \ como se espera mediante controles automatizados. Los programadores lo hacen\u2026"
lastmod: 2024-02-19 22:05:17.629437
model: gpt-4-0125-preview
summary: "Escribir pruebas en Haskell se trata de asegurar que tus funciones funcionen\
  \ como se espera mediante controles automatizados. Los programadores lo hacen\u2026"
title: Escribiendo pruebas
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas en Haskell se trata de asegurar que tus funciones funcionen como se espera mediante controles automatizados. Los programadores lo hacen para capturar errores temprano, facilitar la refactorización y documentar el comportamiento, haciendo que la base de código sea más mantenible y escalable.

## Cómo hacerlo:

Haskell soporta varios marcos de pruebas, pero dos populares son `Hspec` y `QuickCheck`. Hspec te permite definir especificaciones legibles por humanos para tu código, mientras que QuickCheck te permite generar pruebas automáticamente describiendo propiedades que tu código debería satisfacer.

### Usando Hspec

Primero, añade `hspec` a la configuración de tu herramienta de construcción (por ejemplo, `stack.yaml` o archivo `cabal`). Luego, importa `Test.Hspec` y escribe pruebas como especificaciones:

```haskell
-- archivo: spec/MyLibSpec.hs
import Test.Hspec
import MyLib (add)

main :: IO ()
main = hspec $ describe "MyLib.add" $ do
  it "suma dos números" $
    add 1 2 `shouldBe` 3

  it "devuelve el primer número al sumar cero" $
    add 5 0 `shouldBe` 5
```

Luego, ejecuta tus pruebas usando tu herramienta de construcción, resultando en una salida que podría verse así:

```
MyLib.add
  - suma dos números
  - devuelve el primer número al sumar cero

Terminado en 0.0001 segundos
2 ejemplos, 0 fallos
```

### Usando QuickCheck

Con QuickCheck, expresas propiedades que tus funciones deberían satisfacer. Añade `QuickCheck` a la configuración de tu proyecto, luego impórtalo:

```haskell
-- archivo: test/MyLibProperties.hs
import Test.QuickCheck
import MyLib (add)

prop_addAssociative :: Int -> Int -> Int -> Bool
prop_addAssociative x y z = x + (y + z) == (x + y) + z

prop_addCommutative :: Int -> Int -> Bool
prop_addCommutative x y = x + y == y + x

main :: IO ()
main = do
  quickCheck prop_addAssociative
  quickCheck prop_addCommutative
```

Ejecutar estas pruebas generará automáticamente entradas para comprobar las propiedades especificadas:

```
+++ OK, pasó 100 pruebas.
+++ OK, pasó 100 pruebas.
```

En ambos ejemplos, de Hspec y QuickCheck, los conjuntos de pruebas sirven como documentación ejecutable que puede verificar automáticamente la corrección de tu código.
