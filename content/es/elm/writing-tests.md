---
title:                "Elm: Escribiendo pruebas"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-tests.md"
---

{{< edit_this_page >}}

¿Por qué escribir pruebas en Elm?

Es muy importante escribir pruebas en Elm para garantizar la calidad y estabilidad de nuestro código. Las pruebas nos permiten identificar errores y problemas en nuestro programa antes de que se conviertan en problemas mayores.

## Cómo hacerlo

Para escribir pruebas en Elm, utilizamos el paquete `elm-explorations/test` que viene incluido en Elm por defecto. Primero, necesitamos importar este paquete en nuestro módulo de pruebas, de la siguiente manera:

```Elm
import Test exposing (..)
```

Luego, podemos comenzar a escribir nuestras pruebas utilizando las funciones proporcionadas por el paquete `elm-explorations/test`. Por ejemplo, si queremos probar una función que suma dos números, podríamos escribir una prueba de esta manera:

```Elm
additionTest : Test
additionTest =
  describe "addition"
    [ test "sums two numbers" <|
        \_ -> Expect.equal (addition 2 3) 5
    ]
```

En este ejemplo, estamos utilizando la función `describe` para establecer el contexto de nuestra prueba y la función `test` para definir una prueba específica. Luego, utilizamos la función `Expect.equal` para definir la expectativa que debe cumplirse en nuestra prueba.

Una vez que hayamos escrito todas nuestras pruebas, podemos ejecutarlas utilizando la herramienta de línea de comandos integrada en Elm, utilizando el siguiente comando:

```
elm-test
```

Esto nos mostrará los resultados de nuestras pruebas y nos indicará si han tenido éxito o no.

## Profundizando

Las pruebas en Elm se dividen en dos tipos: pruebas unitarias y pruebas de integración. Las pruebas unitarias se centran en probar funciones y módulos individuales, mientras que las pruebas de integración se enfocan en probar cómo se comunican diferentes componentes de nuestro programa.

Es importante escribir tanto pruebas unitarias como de integración para garantizar que nuestro código funcione correctamente en todos los niveles.

Además, también debemos escribir pruebas para casos límite y para validar nuestros errores y manejo de errores en nuestro código. Esto nos permitirá identificar y corregir problemas potenciales en nuestro programa.

## Ver también

- [Documentación oficial de Elm sobre pruebas](https://guide.elm-lang.org/testing/)
- [Curso de Udemy de pruebas en Elm](https://www.udemy.com/course/testing-in-elm/?referralCode=DECC5136066F9A0701FA)