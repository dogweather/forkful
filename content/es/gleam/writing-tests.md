---
title:                "Escribiendo pruebas"
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Qué & Por qué?
Escribir pruebas es crear código que verifica si otro código funciona como debe. Los programadores hacen esto para evitar errores, garantizar calidad y facilitar mantenimientos futuros.

## Cómo hacerlo:
```Gleam
import gleam/should
import my_module

pub fn add_test() {
  should.equal(my_module.add(1, 1), 2)
}

pub fn subtract_test() {
  should.equal(my_module.subtract(5, 3), 2)
}
```

Salida de ejemplo con éxito:
```
1 test passed.
```

Salida de ejemplo con fallo:
```
1 test failed.

    The value
      4
    should equal
      2
```

## Análisis Profundo
Gleam llegó con la idea de llevar la seguridad en tipos a sistemas distribuidos. A diferencia de otros lenguajes como Elixir, ofrece un sistema de tipo estático que ayuda a escribir pruebas más concisas y evitar errores en tiempo de ejecución. Con herramientas como `gleam/should`, escribir pruebas es sencillo. Otras alternativas son EUnit y ExUnit de Erlang y Elixir respectivamente, pero Gleam tiene la ventaja de verificar tipos en tiempo de compilación.

## Ver También
- Comunidad de Gleam en Reddit: [r/gleamlang](https://www.reddit.com/r/gleamlang/)