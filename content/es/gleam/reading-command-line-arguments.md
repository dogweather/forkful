---
title:    "Gleam: Leyendo argumentos de línea de comandos"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez te has preguntado cómo los programas de línea de comandos obtienen información del usuario? La respuesta es a través de los argumentos de línea de comandos, que son lo que escribimos después del nombre del programa en la terminal. En esta publicación del blog, aprenderás cómo leer y utilizar los argumentos de línea de comandos en Gleam.

## Cómo hacerlo

Para leer argumentos de línea de comandos en Gleam, usamos la función `os.args()` que devuelve una lista de cadenas con los argumentos proporcionados. Por ejemplo, si ejecutamos el siguiente código en la terminal con el comando `gleam run main.gleam arg1 arg2`:

```
import os

fn main() {
  let args = os.args()
  // rest of the code
}
```

La variable `args` contendrá una lista con los valores `["main.gleam", "arg1", "arg2"]`.

Podemos acceder a los argumentos individualmente usando sus índices en la lista. Por ejemplo, para obtener el segundo argumento `arg1`, usaríamos `args[1]`.

Ahora, si queremos imprimir los argumentos a la terminal, podemos utilizar la función `io.println()` de la biblioteca estándar de Gleam. El código se vería así:

```
import io
import os

fn main() {
  let args = os.args()
  io.println("Argumento 1:", args[1])
  io.println("Argumento 2:", args[2])
}
```

La salida en la terminal sería:

```
Argumento 1: arg1
Argumento 2: arg2
```

¡Así de simple es leer y utilizar argumentos de línea de comandos en Gleam!

## Profundizando

Ahora que ya sabes cómo leer argumentos de línea de comandos en Gleam, te preguntarás ¿qué más puedo hacer con ellos? Bueno, podemos utilizar los argumentos para crear programas más interactivos e incluso procesar archivos. También podemos agregar validaciones y opciones para facilitar el uso de nuestro programa.

Otra cosa interesante que podemos hacer es utilizar la biblioteca `os` para obtener información del sistema, como el directorio actual o el nombre de usuario, y combinarla con los argumentos de línea de comandos en nuestro programa.

## Ver también

- Documentación oficial de los argumentos de línea de comandos en Gleam: https://gleam.run/documentation/tour/arguments.md
- Ejemplos de uso de argumentos de línea de comandos en programas Gleam: https://github.com/gleam-lang/gleam/blob/master/examples/09_arguments/
- Más información sobre la biblioteca estándar de Gleam: https://gleam.run/documentation/standard_library.md#os