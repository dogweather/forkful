---
title:                "Escribiendo pruebas"
html_title:           "Gleam: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en Gleam?

Escribir pruebas en Gleam es una forma eficiente de verificar el correcto funcionamiento de nuestro código. Nos permite encontrar errores y realizar modificaciones de forma rápida y sencilla, asegurándonos de que nuestro código esté lo más libre de errores posible.

## Cómo escribir pruebas en Gleam

Escribir pruebas en Gleam es sencillo y no requiere demasiado esfuerzo. A continuación, se mostrará un ejemplo básico de una prueba en Gleam:

```Gleam
test "Suma de dos números" {
  expect(Number.add(2, 3)) == 5
}
```

En este ejemplo, estamos probando la función `Number.add` para asegurarnos de que su resultado sea igual a 5 cuando se le pasan los valores 2 y 3 como argumentos. Para ejecutar esta prueba, simplemente debemos utilizar el comando `gleam test` en la terminal y veremos el resultado en la consola.

Otro aspecto importante al escribir pruebas en Gleam es utilizar el módulo `gleam/assert` para realizar nuestras afirmaciones. Este módulo nos proporciona funciones como `expect` y `assert` que nos permiten comparar valores y verificar si se cumplen nuestras condiciones.

## Profundizando en la escritura de pruebas en Gleam

Escribir pruebas en Gleam no solo nos permite verificar el correcto funcionamiento de nuestro código, sino que también nos ayuda a mantenerlo organizado y documentado. Al escribir pruebas, estamos obligados a pensar en todas las posibles situaciones y a documentar nuestro código para que sea más legible y comprensible.

Además, Gleam utiliza un enfoque de programación funcional, lo que significa que nuestras pruebas deben ser puras, es decir, no deben tener efectos secundarios y su resultado debe ser siempre el mismo para los mismos argumentos. Esto nos obliga a escribir código más limpio y fácil de probar.

## Ver también

- [Documentación de pruebas en Gleam](https://gleam.run/book/intro.html#testing)
- [Escribiendo pruebas en Gleam](https://bloggleam.com/writing-tests-in-gleam/) (en inglés)