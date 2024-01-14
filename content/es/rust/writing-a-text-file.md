---
title:                "Rust: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué escribir un archivo de texto en Rust

Si estás interesado en aprender Rust, una de las tareas básicas que deberías saber es cómo escribir un archivo de texto. Los archivos de texto son una forma común de guardar y leer información en cualquier lenguaje de programación, por lo que es importante saber cómo hacerlo en Rust también.

## Cómo escribir un archivo de texto en Rust

Para escribir un archivo de texto en Rust, primero debes abrir un archivo en modo escritura y luego escribir la información que desees guardar en él. Esto se puede hacer utilizando el método `File::create()` para crear un nuevo archivo y el método `write_all()` para escribir en él.

```rust
use std::fs::File;
use std::io::Write;

fn main() {
    // Creamos un nuevo archivo llamado "ejemplo.txt"
    let mut archivo = File::create("ejemplo.txt")
        .expect("No se pudo crear el archivo");

    // Escribimos una cadena de texto en el archivo
    archivo.write_all(b"Hola, esto es un ejemplo de archivo de texto escrito en Rust!")
        .expect("No se pudo escribir en el archivo");

    // Finalizamos el programa
    println!("Archivo de texto escrito exitosamente!");
}
```

El código anterior creará un archivo llamado "ejemplo.txt" en la misma ubicación que el archivo de Rust y escribirá la cadena de texto en él. Si ejecutas este código varias veces, notarás que cada vez que se ejecuta, el archivo se sobrescribe con la nueva información.

## Profundizando en la escritura de archivos de texto en Rust

Si deseas tener un mayor control sobre cómo se escribe la información en un archivo de texto, hay algunas cosas adicionales que puedes hacer. Por ejemplo, si quieres agregar texto a un archivo existente en lugar de sobrescribirlo, puedes usar el método `File::open()` para abrir el archivo en modo de apertura y luego usar el método `write_all()` con la bandera `File::append()` para agregar información nueva al final del archivo.

También es importante conocer diferentes métodos para convertir tipos de datos en cadenas de texto, ya que solo se pueden escribir cadenas en un archivo de texto. Puedes usar el método `.to_string()` para convertir un tipo de dato en una cadena o el macro `format!()` para crear una cadena con múltiples variables.

## Ver también

- [Documentación oficial de Rust sobre manipulación de archivos](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial de escritura de archivos en Rust](https://www.educative.io/edpresso/how-to-write-to-a-file-in-rust)
- [Ejemplos de escritura de archivos en Rust](https://www.tutorialspoint.com/rust/rust_file_io.htm)