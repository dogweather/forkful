---
title:                "Rust: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué

Escribir a la salida de error estándar (standard error) puede ser útil en situaciones en las que queramos mostrar mensajes de error o información de depuración en nuestra aplicación. También puede ser útil para capturar y registrar errores en tiempo de ejecución.

## Cómo hacerlo

Para escribir a la salida de error estándar en Rust, podemos utilizar la macro `eprintln!()`. Esta macro funciona de manera similar a `println!()` pero en lugar de imprimir en la salida estándar, imprime en la salida de error estándar.

Veamos un ejemplo:

```Rust
fn main() {
    let age = 25;

    eprintln!("La edad es: {}", age);
}
```
**Salida:**
```
La edad es: 25
```

## Inmersión profunda

En Rust, la salida de error estándar se representa como un objeto de tipo `io::Stderr`. Esto nos permite utilizar algunos métodos adicionales para formatear o escribir en la salida de error.

Si queremos imprimir un mensaje de error personalizado, podemos utilizar el método `write()` en lugar de `eprintln!()`. Esto nos permitirá formatear el mensaje antes de escribirlo en la salida de error estándar.

```Rust
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let file = std::fs::File::open("archivos.txt")?;

    io::stdout().write(b"¡Archivo abierto correctamente!")?;

    Ok(())
}
```

**Salida:**
```
¡Archivo abierto correctamente!
```

## Ver también

- [Documentación oficial de Rust sobre la macro `eprintln!()`](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Documentación oficial de Rust sobre la salida de error estándar](https://doc.rust-lang.org/std/io/struct.Stderr.html)