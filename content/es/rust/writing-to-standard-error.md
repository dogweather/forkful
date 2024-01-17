---
title:                "Escribiendo al error estándar"
html_title:           "Rust: Escribiendo al error estándar"
simple_title:         "Escribiendo al error estándar"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir al estándar de error (standard error en inglés) en Rust es una forma de mostrar mensajes de error o diagnósticos a los usuarios del programa. Los desarrolladores lo hacen para ayudar a entender y depurar el código más fácilmente.

## Cómo:
La sintaxis básica para escribir al estándar de error en Rust es la siguiente:
```Rust
eprintln!("Mensaje de error");
```
Esto imprimirá el mensaje de error en la consola, aunque también se puede redirigir a un archivo o a cualquier otro destino de salida. Ejemplo de salida:
```Rust
#![deny(warnings)]
use std::io::Error;

fn main() -> Result<(), Error> {
    eprintln!("¡El archivo no pudo ser abierto!");

    Ok(())
}
```
Salida:
```
¡El archivo no pudo ser abierto!
```

## Profundizando:
Escribir al estándar de error no es algo específico de Rust, sino que es una práctica común en otros lenguajes de programación. También se puede hacer con la macro `println!`, pero esto imprimirá el mensaje en la salida estándar (standard output en inglés). Además, se puede combinar con la función `std::io::Write` para especificar exactamente dónde se desea imprimir el mensaje de error.

## Véase también:
- [Documentación de Rust sobre imprimir al estándar de error](https://doc.rust-lang.org/std/io/fn.stderr.html)
- [Artículo sobre escribir al estándar de error en C++](https://www.educative.io/edpresso/how-to-output-to-standard-error-in-cpp)