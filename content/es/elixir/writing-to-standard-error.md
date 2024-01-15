---
title:                "Escribir en la salida estándar de error"
html_title:           "Elixir: Escribir en la salida estándar de error"
simple_title:         "Escribir en la salida estándar de error"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error

Escribir a la salida de error, también conocido como stderr, es una forma útil de comunicarse con los desarrolladores y usuarios durante la ejecución de un programa. Puede proporcionar información sobre posibles errores o excepciones que hayan ocurrido, facilitando la depuración y el seguimiento del estado del programa mientras se ejecuta.

## Cómo escribir a la salida de error

La forma más sencilla de escribir a la salida de error en Elixir es utilizando la función `IO.puts/2` junto con el macro `IO.inspect/2`. Estos métodos aceptan un mensaje y lo escriben en la salida de error, con la diferencia de que `IO.inspect/2` también imprime información adicional sobre la estructura de datos pasada como argumento. Por ejemplo:

```Elixir
IO.puts("Este es un mensaje de error")
```
```Elixir
IO.inspect({:error, "Ocurrió un error"})
```

Salida:

```sh
Este es un mensaje de error
{:error, "Ocurrió un error"}
```

También se puede utilizar la función `IO.write/2` para escribir datos binarios en la salida de error.

## Detalles sobre escribir a la salida de error

Además de escribir mensajes de error simples, también se puede utilizar la función `IO.format/2` para formatear y escribir mensajes en la salida de error. Esta función acepta una cadena de formato similar a la función `printf` en otros lenguajes, lo que permite personalizar los mensajes de error con información específica del programa.

Otro aspecto importante a tener en cuenta es que la salida de error es un flujo de salida como cualquier otro en Elixir, lo que significa que se pueden aplicar operaciones de Elixir como filtrado, mapeo o reducción en él. Esto brinda la flexibilidad de manipular la salida de error de acuerdo a las necesidades específicas del programa.

## Ver también

- [Documentación oficial de Elixir sobre la salida de error](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Ejemplo de uso de la salida de error en Elixir](https://hexdocs.pm/elixir/A.html#error_1)