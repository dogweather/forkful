---
title:                "Rust: Creando un archivo temporal"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué

La creación de archivos temporales es una tarea común en el desarrollo de software. Estos archivos son útiles para almacenar datos temporales o para realizar pruebas en el código sin afectar los archivos existentes. En Rust, la creación de archivos temporales es sencilla y eficiente gracias a las funciones predefinidas en el lenguaje.

## Cómo hacerlo

La creación de un archivo temporal en Rust se realiza en tres pasos básicos. Primero, importamos la librería estándar de Rust `std::fs` que contiene las funciones necesarias para trabajar con archivos. Luego, usamos la función `std::fs::File::create` para crear el archivo temporal y asignarlo a una variable. Por último, escribimos los datos en el archivo y lo cerramos usando el método `File::write_all` y `File::flush`.

```Rust
use std::fs::File;

let mut file = File::create("temp.txt").expect("No se pudo crear el archivo temporal");

file.write_all(b"Hola mundo!").expect("No se pudieron escribir los datos en el archivo");
file.flush().expect("No se pudo cerrar el archivo temporal correctamente");
```

El código anterior creará un archivo llamado "temp.txt" en el directorio actual y escribirá "Hola mundo!" en él. Podemos realizar cualquier operación de escritura en este archivo temporal como lo haríamos en cualquier otro archivo.

Otra manera de crear un archivo temporal en Rust es usando el tipo `std::io::BufWriter`. Este tipo nos permite escribir grandes cantidades de datos en el archivo temporal de manera eficiente.

```Rust
use std::fs::File;
use std::io::BufWriter;

let mut file = BufWriter::new(File::create("temp.txt").expect("No se pudo crear el archivo temporal"));

for i in 0..10000 {
    // Escribir un número en cada línea del archivo
    write!(file, "{}\n", i).expect("No se pudieron escribir los datos en el archivo");
}

// Es importante cerrar el archivo temporal
file.flush().expect("No se pudo cerrar el archivo temporal correctamente");
```

## Profundizando

Ahora que sabemos cómo crear archivos temporales en Rust, podemos profundizar un poco más en cómo funcionan detrás de escena. Al crear un archivo temporal en Rust, estamos creando un handle (manipulador) que apunta al archivo en el sistema de archivos. Este handle nos permite realizar operaciones de manera eficiente sin tener que abrir y cerrar el archivo constantemente.

Cuando el handle se queda sin uso, es decir, cuando se queda sin variables que lo apunten, el archivo temporal se elimina automáticamente del sistema de archivos. Esto es especialmente útil en casos donde solo necesitamos el archivo temporal en un contexto particular y no queremos preocuparnos por eliminarlo manualmente.

## Ver también

- [Documentación oficial de Rust sobre manejo de archivos](https://doc.rust-lang.org/std/fs/)
- [Ejemplo completo de creación de archivo temporal en Rust](https://gist.github.com/adrian-gutierrez/8b5e4e7b47fcd7a0303c878e53af0dd8)