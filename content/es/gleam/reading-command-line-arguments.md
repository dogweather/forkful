---
title:                "Gleam: Leyendo argumentos de línea de comandos"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer los argumentos de línea de comandos en Gleam?

Los argumentos de línea de comandos son una forma poderosa de interactuar con un programa desde la terminal, lo que permite a los usuarios proporcionar información específica para que el programa se ejecute de diferentes maneras. En Gleam, leer los argumentos de línea de comandos es una habilidad esencial para desarrollar aplicaciones flexibles y personalizables.

## Cómo leer los argumentos de línea de comandos en Gleam

Para leer los argumentos de línea de comandos en Gleam, utilizamos la función `command_line.arguments/0`. Esta función devuelve una lista de cadenas con todos los argumentos pasados al programa al ejecutarlo desde la terminal.

```Gleam
import gleam/command_line

fn main() {
  let args = command_line.arguments()

  // Imprimir cada argumento en una nueva línea
  for arg in args {
    io.println(arg)
  }
}
```

Si ejecutamos este programa con el comando `gleam run app.gleam uno dos tres`, obtendremos la siguiente salida:

```
uno
dos
tres
```

## Profundizando en la lectura de argumentos de línea de comandos en Gleam

Además de la función `arguments/0`, Gleam también proporciona otras funciones útiles para trabajar con argumentos de línea de comandos. Por ejemplo, la función `command_line.flag/2` permite leer argumentos que comienzan con un "-".

```Gleam
import gleam/command_line

fn main() {
  let option = command_line.flag("-o", "default")

  // Si no se proporciona el argumento "-o", se usará el valor predeterminado
  // de "default"
  io.println(option)
}
```

Al ejecutar este programa con `gleam run app.gleam -o test`, obtendremos la siguiente salida:

```
test
```

Además de estas funciones básicas, es importante tener en cuenta que los argumentos de línea de comandos son sensibles a mayúsculas y minúsculas en Gleam, por lo que `command_line.flag("-o", ...)` y `command_line.flag("--O", ...)` serán dos argumentos diferentes.

## Ver también

- Documentación oficial de Gleam sobre comandos de línea: https://gleam.run/book/tour/command_line.html
- Ejemplo de aplicación con argumentos de línea de comandos: https://github.com/gleam-lang/examples/blob/master/command-line-arguments/app.gleam