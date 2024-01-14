---
title:                "Gleam: Escribiendo pruebas"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en Gleam

Escribir pruebas es una parte importante del proceso de programación en cualquier lenguaje, y Gleam no es una excepción. Las pruebas permiten verificar el funcionamiento correcto de nuestro código y nos brindan la confianza necesaria para realizar cambios en nuestro código sin temor a introducir errores. 

## Cómo escribir pruebas en Gleam

Las pruebas en Gleam se escriben utilizando el módulo `assert` y las palabras clave `assert` y `expect`. Por ejemplo, si queremos probar que la suma de dos números es correcta, podemos escribir lo siguiente en un archivo de pruebas:

```Gleam
import assert.{equal}

test_addition() {
  assert.equal(2 + 2, 4)
}
```

La función `equal` del módulo `assert` toma dos argumentos y verifica que sean iguales, en caso contrario, la prueba fallará y mostrará un mensaje de error.

Otra forma de escribir pruebas en Gleam es utilizando la palabra clave `expect`. Esta palabra clave toma dos argumentos, el primero es el resultado que esperamos obtener y el segundo es la expresión que se evalúa para obtener el resultado. Por ejemplo:

```Gleam
import assert.{expect}

test_subtraction() {
  expect(10, 20 - 10)
}
```

En este caso, la prueba pasará si el resultado de `20 - 10` es igual a 10.

## Profundizando en la escritura de pruebas

Además de las funciones de `assert` y `expect`, Gleam también ofrece la posibilidad de utilizar patrones de igualdad en nuestras pruebas, lo que nos da una mayor flexibilidad en la escritura de las mismas. También es posible utilizar macros para generar pruebas automáticamente, lo que es especialmente útil cuando tenemos una gran cantidad de casos de prueba similares.

Otro aspecto importante a considerar al escribir pruebas en Gleam es la cobertura de prueba. Esto se refiere a la cantidad de nuestro código que se está ejecutando en nuestras pruebas. Para garantizar una cobertura adecuada, se pueden utilizar herramientas externas como `coveralls` o `codecov`.

## Ver también

- [Documentación oficial de pruebas en Gleam](https://gleam.run/book/tour/testing.html)
- [Ejemplos de pruebas en Gleam](https://github.com/elixir-lang/gleam/blob/master/lib/stdlib/test/stdlib_test.gleam)
- [Tutorial de pruebas en Gleam](https://medium.com/@jennerson/how-to-code-and-test-gleam-language-df6942b86878)