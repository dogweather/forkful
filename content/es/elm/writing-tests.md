---
title:                "Elm: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Elm

Escribir pruebas es una parte esencial del proceso de desarrollo en Elm. Las pruebas nos permiten detectar y corregir errores en nuestro código antes de llegar a producción, asegurando que nuestro programa funcione de manera correcta y sin problemas. Además, también pueden servir como documentación para entender mejor cómo funciona nuestro código.

## Cómo escribir pruebas en Elm

Para escribir pruebas en Elm, utilizamos el módulo `Test` que viene incluido en la biblioteca estándar. Primero, debemos importarlo en nuestro archivo de pruebas utilizando la siguiente línea de código:

```Elm
import Test
```

Luego, podemos crear nuestras pruebas utilizando la función `test` de la siguiente manera:

```Elm
test "Test de suma" <| \_ ->
    expect <| sumar 2 3
        |> toBe 5
```

En este ejemplo, estamos creando una prueba que verifica que la función `sumar` regresa el resultado esperado al sumar 2 y 3. Usamos la función `expect` para especificar el resultado esperado y luego utilizamos la función `toBe` para compararlo con el resultado real de `sumar 2 3`.

Una vez que tenemos todas nuestras pruebas escritas, podemos ejecutarlas utilizando la función `Test.run` y pasando nuestras pruebas como argumento:

```Elm
main =
    Test.run "Pruebas de suma" testSuite
```

## Profundizando en la escritura de pruebas

Aunque hemos cubierto lo básico de escribir pruebas en Elm, hay muchas otras funciones y técnicas que podemos utilizar para hacer nuestras pruebas más robustas y efectivas. Por ejemplo, podemos utilizar la función `expect` para verificar que una función lance una excepción en un caso determinado. También podemos crear grupos de pruebas utilizando la función `testGroup` para organizar nuestras pruebas de manera más clara.

## Ver también

- [Documentación de elm-test](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test)
- [Artículo sobre cómo escribir pruebas en Elm](https://medium.com/swlh/elm-tests-9a897a471d68)
- [Video tutorial sobre pruebas en Elm](https://www.youtube.com/watch?v=_GlLnFBv4TM)