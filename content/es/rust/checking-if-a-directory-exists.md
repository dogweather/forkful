---
title:                "Rust: Comprobando si existe un directorio"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué

Si eres nuevo en la programación de Rust, es posible que te hayas encontrado con la necesidad de comprobar si un directorio existe. Ya sea para manejar errores o para realizar alguna acción en función de la existencia de un directorio, saber cómo realizar esta tarea es esencial para cualquier programador de Rust. En esta publicación, vamos a ver cómo puedes comprobar si un directorio existe en Rust y profundizar en algunos detalles más técnicos.

## Cómo hacerlo

Para comprobar si un directorio existe en Rust, necesitamos utilizar la función `std::path::Path::exists()`. Esta función devuelve un booleano que indica si el directorio existe o no. Veamos un ejemplo de cómo podríamos utilizar esta función en un programa Rust:

```Rust
use std::path::Path;

fn main() {
    let directory = Path::new("mi_directorio");

    if directory.exists() {
        println!("El directorio existe.");
    } else {
        println!("El directorio no existe.");
    }
}
```

En el código anterior, creamos una variable `directory` que contiene una instancia de un `Path` que apunta al directorio que queremos comprobar. Luego utilizamos la función `exists()` para verificar si el directorio existe o no, y en función de eso, imprimimos un mensaje adecuado.

## Profundizando

Ahora que sabemos cómo comprobar si un directorio existe en Rust, es importante entender cómo funciona esta función. `exists()` es una función de asociación del tipo `Path`, lo que significa que la llamamos en una instancia de `Path` (como en nuestro ejemplo anterior). Pero, ¿qué es `Path`?

`Path` es una estructura que representa una ruta en el sistema de archivos. Tiene una serie de funciones disponibles para trabajar con rutas de archivos y directorios, y `exists()` es una de ellas. Esta función se encarga de verificar si la ruta especificada por `Path` existe o no, y devuelve un booleano en consecuencia.

Además, hay que tener en cuenta que `exists()` no distingue entre archivos y directorios. En otras palabras, si `Path` apunta a un archivo existente, la función devolverá `true` de todas maneras. Si quieres comprobar específicamente si un directorio existe, puedes utilizar la función `is_dir()` en su lugar.

## Ver también

- [Documentación oficial de Rust sobre la estructura `Path`](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Guía de Rust sobre manejo de errores](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
- [Ejemplos prácticos de uso de `Path` en la vida real](https://www.tutorialspoint.com/rust/rust_handling_file_and_directories.htm)

Con esto, ya tienes todo lo necesario para comenzar a comprobar si un directorio existe en tus programas Rust. ¡Hasta la próxima!