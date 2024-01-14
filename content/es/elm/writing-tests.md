---
title:    "Elm: Redacción de pruebas"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Elm

Escribir pruebas en Elm puede parecer una tarea tediosa y repetitiva, pero en realidad es una práctica muy importante para garantizar un código confiable y libre de errores. Al escribir pruebas, podemos detectar y corregir problemas antes de que causen problemas en nuestro código en producción. Además, las pruebas también sirven como una documentación útil para otros desarrolladores que puedan trabajar en el mismo proyecto.

## Cómo escribir pruebas en Elm

Para escribir pruebas en Elm, necesitamos usar el módulo `Test` incluido en la biblioteca de Elm. Este módulo proporciona funciones y tipos de datos para crear casos de prueba y comprobar si los resultados se ajustan a lo que esperamos.

Por ejemplo, supongamos que tenemos una función `double` que simplemente duplica un número dado. Para probar esta función, podemos escribir un caso de prueba usando la función `test` de `Test` de la siguiente manera:

```
Elm:
...
double : Int -> Int
double num =
    num * 2

test "Double function doubles the given number" <|
    \_ ->
        let
            num = 5
        in
            Expect.equal (double num) 10

```

En este ejemplo, estamos creando un caso de prueba llamado "Double function doubles the given number" que comprueba si el resultado de `double 5` es igual a 10. Al ejecutar este caso de prueba, veremos si nuestra función `double` está funcionando correctamente. Si se produce algún error, la prueba fallará y nos dará una indicación de qué pudo haber salido mal.

## Profundizando en la escritura de pruebas en Elm

Además de la función `test`, el módulo `Test` proporciona otras funciones útiles para crear casos de prueba más complejos. Por ejemplo, podemos usar la función `testWith` para pasar valores personalizados a nuestras pruebas y probar diferentes escenarios. También podemos utilizar la función `expectation` para comprobar si una condición es verdadera o falsa.

Además, podemos escribir pruebas para nuestras funciones en un archivo separado para mantener nuestro código organizado y fácil de mantener. Como desarrolladores, también podemos aprovechar otras herramientas como `elm-test` para automatizar la ejecución de nuestras pruebas y recibir notificaciones cuando se producen cambios en nuestro código.

## Ver también

- [Documentación oficial de pruebas en Elm](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Cómo escribir pruebas en Elm: Ejemplo de implementación](https://medium.com/@afcastano/elm-testing-framework-86a6e47e2fb2)
- [Guía para escribir pruebas efectivas en Elm](https://thoughtbot.com/blog/how-to-write-effective-tests-in-elm)