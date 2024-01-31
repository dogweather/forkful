---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"

category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Escribir pruebas consiste en crear casos para comprobar que tu código hace lo que esperas. Los programadores las usan para prevenir errores y asegurar que las nuevas características no rompan las existentes.

## How to:
Elm utiliza `elm-test` para escribir y ejecutar pruebas. A continuación, un ejemplo simple:

```Elm
import Expect
import Test exposing (..)

suite : Test
suite =
  describe "Valores absolutos"
    [ test "Valor absoluto de 3 es 3" <|
        \_ -> Expect.equal 3 (abs 3)
    , test "Valor absoluto de -4 es 4" <|
        \_ -> Expect.equal 4 (abs -4)
    ]

-- Para ejecutar las pruebas:
-- elm-test
```
Salida de muestra cuando las pruebas pasan:
```
TEST RUN PASSED

Valores absolutos
    ✔ Valor absoluto de 3 es 3
    ✔ Valor absoluto de -4 es 4
```

## Deep Dive
Elm siempre ha abogado por la robustez y fiabilidad. La historia de `elm-test` refleja esto, proporcionando una manera de escribir pruebas desde las versiones tempranas del lenguaje. Alternativas a `elm-test` son raras porque la herramienta es integral en la comunidad. Detalles de implementación: `elm-test` usa Node.js y corre en un entorno headless para simular navegadores.

## See Also
- [Elm Test](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [`elm-test` CLI Documentation](https://github.com/elm-explorations/test#running-tests-locally)
- [Elm Programming Language](https://elm-lang.org/)
