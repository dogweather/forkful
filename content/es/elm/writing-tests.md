---
title:                "Escribiendo pruebas"
html_title:           "Elm: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas (tests) en Elm puede parecer una tarea tediosa e innecesaria, pero en realidad puede ser una herramienta muy valiosa para garantizar que nuestro código funcione correctamente y mantenerlo libre de errores. Además, escribir pruebas desde el principio nos permite tener una mayor confianza en nuestro código a medida que vamos añadiendo nuevas funcionalidades.

## Cómo hacerlo

Escribir pruebas en Elm es bastante sencillo y se puede hacer utilizando módulos y funciones específicas de esta herramienta de programación funcional. A continuación, se presenta un ejemplo de cómo escribir una prueba simple para una función de suma:

```elm
import Test exposing (..)
import Expect exposing (expect)

sumar : Int -> Int -> Int
sumar x y =
    x + y

sumaCorrecta : Test
sumaCorrecta =
    describe "Sumar" <|
        test "la suma de dos números debería ser correcta" <|
            \_ ->
                expect <| sumar 2 3
                    |> toBe 5

```

En este ejemplo, estamos importando los módulos `Test` y `Expect`, que nos permiten escribir nuestras pruebas y definir expectativas sobre los resultados. Luego, definimos una función `sumar` que toma dos números enteros y los suma, y una prueba llamada `sumaCorrecta` que describe el comportamiento deseado de esta función. Para finalizar, utilizamos la función `expect` para verificar que el resultado de la suma de 2 y 3 sea igual a 5.

Si corremos esta prueba utilizando alguna herramienta de testeo de Elm, como por ejemplo `elm-test`, deberíamos obtener una salida en la que se indica si la prueba pasó o falló. En este caso, deberíamos recibir una confirmación de que nuestra prueba fue exitosa.

## Profundizando

Escribir pruebas en Elm nos ofrece una serie de beneficios, como la capacidad de detectar errores antes de que lleguen a producción y la posibilidad de refactorizar nuestro código de manera segura. Además, nos permite tener un mayor control y comprensión de nuestro código, ya que nos obliga a pensar en las posibles situaciones en las que nuestro código podría fallar.

Otro aspecto importante a tener en cuenta al escribir pruebas en Elm es tener en cuenta el principio de "una prueba por función". Esto significa que cada función debería tener su propia prueba específica que verifique su funcionamiento. De esta manera, podemos aislar y solucionar errores de manera más eficiente.

Además, podemos utilizar la función `testOnly` para correr solo una prueba específica, lo que nos permite enfocarnos en un determinado aspecto de nuestro código mientras seguimos trabajando en él.

## Ver también

- [Documentación de Elm - Testing](https://guide.elm-lang.org/testing/)
- [Introducción a los tests de Elm](https://medium.com/@joneshf/introduction-to-elm-s-test-package-800c15f56c12)
- [Elm Test - Guía de inicio rápido](https://giang-caprio.gitlab.io/blog/2019/02/10/elm-test-quick-start-guide/)