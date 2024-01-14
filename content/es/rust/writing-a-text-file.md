---
title:                "Rust: Creando un archivo de texto"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto en Rust?

Escribir un archivo de texto en Rust puede ser una tarea útil para almacenar y compartir datos en un formato legible para humanos. Además, puede ser una buena práctica para familiarizarse con el manejo de archivos en el lenguaje de programación Rust.

## Cómo escribir un archivo de texto en Rust

Para escribir un archivo de texto en Rust, primero debemos importar el módulo `std::fs`, que nos proporciona funciones para interactuar con el sistema de archivos. Luego, podemos utilizar la función `write` del módulo para escribir nuestro texto en el archivo. Veamos un ejemplo:

```Rust 
use std::fs::File;
use std::io::prelude::*;

// Crear un nuevo archivo llamado "blogpost.txt"
let mut archivo = File::create("blogpost.txt").expect("No se pudo crear archivo.");

// Escribir el texto "¡Bienvenidos al blog de Rust!" en el archivo
archivo.write(b"¡Bienvenidos al blog de Rust!").expect("No se pudo escribir en el archivo.");
```

Este código creará un archivo llamado "blogpost.txt" en la misma ubicación donde se está ejecutando el programa, y escribirá el texto proporcionado en él. También es importante destacar el uso de `expect`, que nos permite manejar posibles errores que puedan ocurrir al crear o escribir en el archivo.

## Profundizando en la escritura de archivos de texto en Rust

La función `write` nos permite escribir cadenas de bytes (también conocidas como slices) en el archivo, pero ¿qué pasa si queremos escribir texto en su lugar? Podemos utilizar la función `write_fmt`, que nos permite formatear y escribir texto en el archivo utilizando la sintaxis similar a `println!`. Veamos un ejemplo:

```Rust
archivo.write_fmt(format_args!("¡Hoy es {}", "un gran día")).expect("No se pudo escribir en el archivo.");
```

Esta función nos permite reutilizar los conceptos aprendidos al utilizar `println!` para imprimir en la consola, pero en lugar de eso, escribiremos en el archivo. También podemos utilizar el método `write_all` para escribir todo el contenido de una sola vez en lugar de en fragmentos.

## Ver También

- [Documentación de std::fs en Rust](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial sobre manejo de archivos en Rust](https://www.tutorialspoint.com/rust/rust_file_management.htm)
- [Introducción a Rust: Manipulación de archivos y breve mirada a la gestión de errores](https://www.packtpub.com/programming/introducing-rust-file-manipulation-and-brief-look-error-handling)