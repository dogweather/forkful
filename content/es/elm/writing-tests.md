---
title:                "Escribir pruebas"
html_title:           "Elm: Escribir pruebas"
simple_title:         "Escribir pruebas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas es una práctica común en la programación. Básicamente, consiste en crear pequeños programas que verifican si una parte específica de nuestro código funciona como debería. Es una forma de asegurarse de que todo está funcionando correctamente y de detectar posibles errores antes de que se conviertan en problemas más grandes.

Los programadores escriben pruebas por varias razones, entre ellas: garantizar un código más limpio y menos propenso a errores, facilitar la detección y corrección de errores, y mejorar la calidad y estabilidad del código en general.

## Cómo hacerlo:

```Elm
probandoCadenas : () -> Test
probandoCadenas =
  test "Probando funciones de cadenas" <|
    \_ ->
      describe "Concatenando cadenas" <|
        [ test "Concatenación exitosa" <|
            \_ ->
              let
                string1 = "¡Hola "
                string2 = "mundo!"
                resultado = String.concat [string1, string2]
              in
                Expect.equal resultado "¡Hola mundo!"
        , test "Concatenación con número" <|
            \_ ->
              let
                string1 = "Tengo "
                numero = 10
                string2 = " años"
                resultado = String.concat [string1, numero, string2]
              in
                Expect.equal resultado "Tengo 10 años"
        ]
```

Resultado:

```Elm
Test ID: 20701d66-3495-43a5-9763-d77f15edc244 ~

√ Probando funciones de cadenas:Passed - 2 suites and 2 tests
√ Test suite "Concatenación exitosa":Passed - 1 test
√ Test "Concatenación exitosa":Passed - Equality Check
√ Test suite "Concatenación con número":Passed - 1 test
√ Test "Concatenación con número":Passed - Equality Check
```

## Inmersión profunda:

La escritura de pruebas tiene su origen en la metodología de desarrollo de software conocida como "Desarrollo guiado por pruebas" o Test-driven development (TDD). Sin embargo, también se utiliza en otras metodologías ágiles como el "Desarrollo orientado a pruebas" o Behaviour-driven development (BDD). En ambas, el objetivo es el mismo: asegurar que el código desarrollado cumpla con los requisitos previamente establecidos.

Otra forma de realizar pruebas en Elm es utilizando Debug.log, una función que permite imprimir valores y mensajes en la consola del navegador para comprobar el funcionamiento de nuestro código en tiempo de ejecución. Sin embargo, esta técnica no es recomendable para pruebas complejas, ya que puede ser difícil de mantener y no ofrece la misma precisión que las pruebas unitarias.

## Ver más:

- Documentación de pruebas en Elm: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Ejemplos de pruebas en Elm: https://github.com/elm/community-resources/tree/master/examples/testing
- TDD vs BDD: https://medium.com/@nosolopau/tdd-vs-bdd-like-chalk-and-cheese-b9a80655813a