---
title:                "Rust: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué
Los argumentos de línea de comando son una herramienta útil para permitir que los usuarios interactúen con un programa desde la línea de comandos. Aprender a leerlos en Rust te permitirá crear programas más versátiles y fáciles de usar para tus usuarios.

## Cómo hacerlo
Para leer los argumentos de línea de comando en Rust, primero debes importar el módulo `std::env`. Luego, puedes utilizar el método`args()` para obtener un iterador que contiene todos los argumentos proporcionados en la línea de comando. A continuación, puedes utilizar un ciclo `for` para recorrer todos los argumentos y realizar cualquier acción que necesites.

```Rust
use std::env;

fn main() {
    let args = env::args();
    for argumento in args {
        println!("{}", argumento);
    }
}
```

Si ejecutas este programa con los argumentos `rust blog` en la línea de comando, la salida será:

```
rust
blog
```

Puedes acceder a los argumentos individuales utilizando su índice en el iterador `args`. Por ejemplo, el primer argumento tendría el índice `1` ya que el índice `0` es el nombre del programa.

```Rust
use std::env;

fn main() {
    let args = env::args();
    let primer_argumento = args.nth(1).expect("Falta el primer argumento");
    println!("El primer argumento es {}", primer_argumento);
}
```

La función `nth()` devuelve el argumento en el índice especificado y elimina ese argumento del iterador. Si el índice es mayor que el número de argumentos, devuelve un error.

## Profundizando
Hay muchas otras formas de leer y manejar los argumentos de línea de comando en Rust, como utilizar argumentos opcionales o validar los argumentos proporcionados por el usuario. Puedes leer más sobre esto en la documentación oficial de Rust o buscar ejemplos de código en línea.

## Vea también
- [Documentación oficial de Rust sobre `std::env`](https://doc.rust-lang.org/std/env/index.html)
- [Ejemplos de código para leer argumentos de línea de comando en Rust](https://www.conwaydev.com/working-with-command-line-arguments-in-rust/)