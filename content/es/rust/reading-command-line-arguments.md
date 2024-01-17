---
title:                "Leyendo argumentos de línea de comando"
html_title:           "Rust: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¡Qué y Por qué?

Leer los argumentos de la línea de comandos es una técnica común en la programación para permitir a los usuarios ingresar datos o configurar un programa al ejecutarlo. Los programadores lo hacen para hacer que sus programas sean más interactivos y personalizables para los usuarios.

## ¡Cómo hacerlo!

Para leer argumentos de la línea de comandos en Rust, se puede utilizar la estructura `std::env::args()`. Aquí hay un ejemplo de cómo hacerlo:

```Rust
use std::env;

fn main() {
    // Obtener los argumentos de la línea de comandos
    let args: Vec<String> = env::args().collect();
    
    // Iterar a través de los argumentos y mostrarlos en la consola
    for arg in args.iter() {
        println!("{}", arg);
    }
}
```

**Salida de muestra:**

```
$ mi_programa hola mundo
hola
mundo
```

## Inmersión Profunda

Leer argumentos de la línea de comandos ha sido una técnica utilizada por los programadores desde los primeros días de la programación. En Rust, también se puede utilizar el crate `structopt` para leer argumentos y configurar opciones de línea de comandos de manera más avanzada.

Otra alternativa es utilizar bibliotecas externas como `clap` o `getopts`. Ambas ofrecen una variedad de funciones para facilitar la lectura y el manejo de argumentos de la línea de comandos.

En cuanto a la implementación, la estructura `env::args()` en Rust devuelve un `std::env::Args` que contiene una referencia a los argumentos en forma de cadenas. Puede iterar a través de este objeto o utilizar funciones como `next()` o `nth()` para obtener argumentos específicos.

## Ver También

- [Documentación oficial de Rust sobre `std::env`](https://doc.rust-lang.org/std/env/index.html)
- [Crate `structopt` para manejar argumentos de la línea de comandos de manera avanzada en Rust](https://crates.io/crates/structopt)
- [Crate `clap` para manejar argumentos de la línea de comandos en Rust](https://crates.io/crates/clap)
- [Crate `getopts` para manejar argumentos de la línea de comandos en Rust](https://crates.io/crates/getopts)