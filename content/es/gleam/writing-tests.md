---
title:    "Gleam: Redacción de pruebas"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Por qué escribir pruebas en Gleam es importante

Escribir pruebas en Gleam puede ser una tarea tediosa, ¡pero es increíblemente importante para garantizar que tu código funcione correctamente! Las pruebas nos permiten detectar errores y nos aseguran de que nuestro código hace lo que debería hacer.

## Cómo escribir pruebas en Gleam

Para escribir pruebas en Gleam, debemos primero importar el módulo de pruebas en nuestro archivo de código fuente:

```Gleam
import testing
```

Luego, podemos definir nuestras pruebas utilizando la función `test` con un nombre descriptivo y una función que contenga nuestras comprobaciones:

```Gleam
test("Comprobar que 2 + 2 es igual a 4", fn () {
  assert_equal(2 + 2, 4)
})
```

En este ejemplo, estamos utilizando la función `assert_equal` para comprobar si el resultado de la suma es igual a 4. Si la comprobación falla, nuestro test fallará y se nos informará del error.

Podemos ejecutar nuestras pruebas utilizando el comando `gleam test` en la terminal y recibiremos una respuesta como esta:

```Shell
$ gleam test

Running....

Test 1: Comprobar que 2 + 2 es igual a 4     OK 
```

¡Genial! Ahora sabemos que nuestro código funciona correctamente. Además de la función `assert_equal`, existen otras funciones de prueba que podemos utilizar, como `assert_not_equal`, `assert_true` y `assert_false`, entre otras.

## Profundizando en la escritura de pruebas

Las pruebas son una parte importante del desarrollo de software y existen diferentes estrategias y técnicas para escribirlas. Algunas recomendaciones para escribir pruebas efectivas en Gleam incluyen:

- Asegúrate de que tus pruebas cubran todos los casos posibles.
- Utiliza nombres descriptivos y claros para tus pruebas.
- Divide tus pruebas en diferentes funciones para mantenerlas organizadas.
- Incluye comentarios para explicar el propósito de cada prueba y qué se espera.
- Utiliza datos de prueba específicos y relevantes para tus comprobaciones.

Recuerda que no hay una única forma correcta de escribir pruebas en Gleam, ¡así que experimenta y encuentra la que mejor se adapte a tu estilo de programación!

## Ver también
- [Documentación de pruebas en Gleam](https://gleam.run/documentation/stdlib/testing/)
- [Estrategias para escribir pruebas efectivas](https://www.softwaretestinghelp.com/how-to-write-good-test-cases-examples-types-tips/)
- [Tutorial introductorio a Gleam](https://dev.to/joelkemp/taking-a-deep-dive-in-the-shiny-new-gleam-programming-language-67b)

¡Ahora es tu turno de poner en práctica la escritura de pruebas en Gleam! Recuerda que mientras más pruebas escribas, más confianza tendrás en tu código y en su correcto funcionamiento. ¡Hasta la próxima!