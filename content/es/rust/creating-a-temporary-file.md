---
title:    "Rust: Creando un archivo temporal"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

La creación de archivos temporales es una práctica común en la programación para almacenar información de forma temporal durante la ejecución de un programa. Esto puede ser útil para realizar pruebas, guardar datos intermedios o simplemente para manejar datos que no son necesarios una vez que el programa ha finalizado su ejecución.

## Cómo hacerlo

En Rust, podemos utilizar la función `std::fs::File::create` para crear un archivo vacío y luego escribir datos en él. Para crear un archivo temporal, podemos utilizar la función `tempfile::tempfile` de la biblioteca `tempfile`. Veamos un ejemplo básico de cómo crear y escribir en un archivo temporal en Rust:

```Rust
use std::fs::File;
use std::io::prelude::*;
use tempfile::tempfile;

fn main() {
    // Crear un archivo temporal
    let mut file = tempfile().expect("No se pudo crear el archivo temporal");

    // Escribir datos en el archivo
    file.write_all(b"Hola, esto es un ejemplo de archivo temporal").expect("No se pudo escribir en el archivo");

    // Leer el archivo temporal
    let mut contents = String::new();
    file.read_to_string(&mut contents).expect("No se pudo leer el archivo");
    println!("{}", contents); // Imprime "Hola, esto es un ejemplo de archivo temporal"
}
```

En este ejemplo, estamos utilizando la biblioteca estándar `std::fs` para crear un archivo temporal y la biblioteca `tempfile` para hacer que la creación del archivo sea más sencilla. Luego, escribimos datos en el archivo utilizando el método `write_all` y leemos su contenido utilizando el método `read_to_string`.

## Profundizando

Si queremos tener un mayor control sobre la creación de archivos temporales, podemos utilizar la función `tempfile::Builder::new`. Con esta función, podemos especificar la ubicación de la carpeta en la que se creará el archivo, el nombre y la extensión del archivo, entre otras opciones. Además, podemos utilizar la función `tempfile::NamedTempFile::create` para crear un archivo temporal con un nombre determinado. Estas opciones pueden ser útiles para casos específicos en los que necesitamos tener un mayor control sobre la creación de archivos temporales.

## Véase también

- Documentación oficial de Rust sobre la creación de archivos (`std::fs::File`): https://doc.rust-lang.org/std/fs/struct.File.html
- Biblioteca `tempfile` utilizada en este artículo: https://docs.rs/crate/tempfile/3.2.0
- Ejemplos de uso de `tempfile::Builder::new`: https://docs.rs/tempfile/3.2.0/tempfile/struct.Builder.html