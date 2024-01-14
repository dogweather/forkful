---
title:    "Gleam: Leyendo argumentos de línea de comando"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

En esta publicación, exploraremos la importancia de leer argumentos de línea de comando en la programación Gleam. Aprenderemos cómo esta habilidad puede ser útil para crear aplicaciones más dinámicas y flexibles. ¡Sigue leyendo para descubrir más!

## Cómo hacerlo

La lectura de argumentos de línea de comando en Gleam es muy sencilla. Simplemente sigue estos pasos:

1. Primero, importa el módulo Gleam `os` en tu archivo de código.
```Gleam
import os
```
2. A continuación, utiliza la función `args()` del módulo `os` para obtener una lista de todos los argumentos pasados al programa en la línea de comando.
```Gleam
let arguments = os.args()
```
3. Puedes iterar sobre esta lista para acceder a cada argumento individualmente y realizar acciones en base a ellos. Por ejemplo, si quieres imprimir una lista de todos los argumentos pasados, puedes hacer lo siguiente:
```Gleam
for argument in arguments {
  io.print(argument)
}
```
Mira cómo se ve esto en acción en el siguiente ejemplo de código:

```Gleam
import os
import io

fn main() {
  let arguments = os.args()
  for argument in arguments {
    io.print(argument)
  }
}
```
Ahora, si pasamos algunos argumentos al ejecutar este programa desde la línea de comando, podemos ver cómo se imprimen en el terminal.

```bash
$ gleam run my_program.gleam argument1 argument2 argument3
```
```
argument1
argument2
argument3
```

## Profundizando

La habilidad de leer argumentos de línea de comando es especialmente útil cuando se trata de crear aplicaciones que necesitan personalización o configuración por parte del usuario. Por ejemplo, si estás creando una aplicación de línea de comandos que realiza cálculos matemáticos para el usuario, puedes permitirles pasar opciones o números específicos como argumentos al programa, en lugar de tener que codificarlos directamente en el código.

Además, leer argumentos de línea de comando también te da la posibilidad de utilizar tu creatividad para implementar diferentes funcionalidades en tus aplicaciones. Puedes utilizar expresiones regulares y otros métodos para procesar y manipular los argumentos recibidos, haciendo que tu programa sea más poderoso y versátil.

## Ver también

- Documentación de Gleam sobre el módulo `os`: https://gleam.run/articles/file-io
- Ejemplos prácticos de lectura de argumentos de línea de comando en Gleam: https://github.com/gleam-lang/gleam/tree/master/examples/command-line-arguments
- Tutoriales y ejercicios para practicar la manipulación de argumentos de línea de comando: https://www.learnshell.org/en/Command_Line_Arguments