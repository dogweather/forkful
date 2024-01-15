---
title:                "Escribiendo a error estándar"
html_title:           "Gleam: Escribiendo a error estándar"
simple_title:         "Escribiendo a error estándar"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado por qué es importante escribir en el error estándar en tus programas? Bueno, aquí te lo explicamos. 

## Cómo hacerlo

La versión actual de Gleam cuenta con una función incorporada para escribir en el error estándar. Es muy sencillo de usar y puede ser muy útil para encontrar errores en tu código. Aquí te mostramos un ejemplo de cómo hacerlo en Gleam:

```Gleam
import gleam/io

fn main() {
  io.stderr("¡Este es un mensaje de error!")
}
```

La salida de este código sería algo como esto:

```
¡Este es un mensaje de error!
```

## Profundizando

Escribir en el error estándar es una práctica común en el mundo de la programación. No solo es útil para identificar errores en tu código, sino que también puede ser útil para imprimir mensajes de advertencia o depuración en tiempo de ejecución. Además, es una forma de comunicarse con el usuario en tiempo real, en lugar de simplemente imprimir en la consola. 

## Ver también

- Documentación oficial de Gleam sobre la escritura en el error estándar: https://gleam.run/articles/writing_to_stderr
- Otras formas de imprimir en la consola en Gleam: https://gleam.run/articles/hello_world#output
- Tutorial de Gleam para principiantes: https://gleam.run/getting-started/