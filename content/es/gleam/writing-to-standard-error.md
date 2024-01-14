---
title:                "Gleam: Escritura en error estándar"
simple_title:         "Escritura en error estándar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# ¿Por qué escribir a la salida de error estándar en Gleam?

La impresión de mensajes de error es una parte importante de cualquier programa. Al escribir a la salida de error estándar en Gleam, se pueden proporcionar mensajes claros y precisos que ayudarán a los desarrolladores a detectar y solucionar errores en su código de manera más eficiente.

## Cómo hacerlo

Para escribir a la salida de error estándar en Gleam, se puede utilizar la función `std.io.write_error/1`. Por ejemplo:

```Gleam
import std.io

fn main() {
    std.io.write_error("¡Este es un mensaje de error!")
}
```
Este código imprimirá el mensaje de error en la consola cuando se compile y ejecute el programa, como se puede ver en la siguiente salida:

```
¡Este es un mensaje de error!
```

## Profundizando

Además de la función `std.io.write_error/1`, Gleam también proporciona la función `std.io.write_error_line/1`, que agrega automáticamente un salto de línea al final del mensaje de error. También se pueden usar caracteres de escape, como `\n`, para agregar saltos de línea en cualquier lugar del mensaje.

Otra característica útil es la posibilidad de formatear mensajes de error utilizando la macro `format!` de Gleam. Esto permite combinar variables y texto para crear mensajes más complejos y personalizados. Por ejemplo:

```Gleam
import std.io

fn main() {
    let name = "Juan"
    let age = 25
    std.io.write_error_line(format!("¡Hola, {}! Tienes {} años de edad.", name, age))
}
```

Salida:

```
¡Hola, Juan! Tienes 25 años de edad.
```

# Ver también

- Documentación oficial de Gleam para `std.io`: https://gleam.run/documentation/stdlib/io
- Ejemplos de uso de la función `std.io.write_error/1`: https://github.com/gleam-lang/gleam_stdlib/blob/master/lib/run_test/src/io_test.gleam#L31