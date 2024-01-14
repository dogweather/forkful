---
title:                "Gleam: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué
Escribir pruebas o tests es una práctica común para garantizar que nuestro código funcione correctamente y evitar posibles errores en el futuro. En lugar de simplemente depender de la revisión manual del código, es importante incluir pruebas en nuestro proceso de desarrollo para asegurarnos de que todo funcione como debería.

## Cómo hacerlo
Para escribir pruebas en Gleam, primero debemos importar el módulo `gleam/test` a nuestro archivo. Luego, podemos utilizar la función `test` para definir nuestras pruebas. Veamos un ejemplo:

```Gleam
import gleam/test

fn add(x, y) {
  x + y
}

test("La suma de 2 y 2 debería ser 4", _ {
  assert add(2, 2) == 4
})
```

En este código, estamos importando el módulo de pruebas y definiendo una función `add` que suma dos valores. Luego, usamos la función `test` para definir una prueba que comprueba si la suma de 2 y 2 es igual a 4. Finalmente, utilizamos la función `assert` para verificar si la afirmación es verdadera.

## Profundizando
Para escribir pruebas más completas, podemos utilizar patrones de correspondencia y expresiones `let` en nuestras `assert`. También podemos agrupar nuestras pruebas utilizando la función `test_suites`. Por ejemplo:

```Gleam
import gleam/test

fn list_length(list) {
  let len = length(list)
  { list, len }
}

let fruits = ["manzana", "naranja", "plátano"]

fn fruits_suite() {
  test_suites("Longitud de la lista", [
    test("La longitud de la lista de frutas debería ser 3", _ {
      let { _, len } = list_length(fruits)
      assert len == 3
    }),
    test("La lista de frutas no debería estar vacía", _ {
      let { list, _ } = list_length(fruits)
      assert list != []
    })
  ])
}
```

En este ejemplo, estamos utilizando un patrón de correspondencia en nuestra `assert` para comprobar la longitud de la lista de frutas y también estamos utilizando la expresión `let` para extraer la lista de frutas y su longitud en diferentes pruebas dentro de una suite de pruebas más grande.

## Ver también
- Documentación de Gleam sobre pruebas (https://gleam.run/book/testing.html)
- Revisión de código de Gleam con pruebas (https://github.com/gleam-lang/gleam/issues/172)