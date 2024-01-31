---
title:                "Comprobando si existe un directorio"
date:                  2024-01-20T14:58:28.114853-07:00
simple_title:         "Comprobando si existe un directorio"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Comprobar si un directorio existe permite evitar errores al intentar acceder a archivos o crear nuevos directorios donde ya hay uno. Es una práctica común para asegurar la robustez del código.

## How to:
Para verificar la existencia de un directorio en Rust, utilizaremos el módulo `std::fs` y `Path`.

```Rust
use std::fs;
use std::path::Path;

fn main() {
    let path = Path::new("/algún/directorio");

    if path.exists() && path.is_dir() {
        println!("¡El directorio existe!");
    } else {
        println!("El directorio no existe.");
    }
}
```

Esta es la salida posible:
```
¡El directorio existe!
```
o
```
El directorio no existe.
```

## Deep Dive
La comprobación de la existencia de un directorio en sistemas de archivos es una funcionalidad básica en la mayoría de los lenguajes de programación, y Rust no es la excepción. Históricamente, esta operación puede ser costosa en términos de rendimiento, por lo que se recomienda usarla con moderación y siempre antes de intentar operaciones en un directorio.

Además de `exists()` y `is_dir()`, también puedes usar la función `std::fs::metadata()` que proporciona más detalles sobre un camino de fichero o directorio. Ahora bien, existe `std::fs::read_dir()` si lo que quieres es no solo comprobar la existencia, sino también leer el contenido del directorio.

Un detalle de la implementación es que `exists()` puede retornar `false` tanto para rutas que no existen como para aquellas a las que no tienes acceso. Esto es importante tener en cuenta para manejar adecuadamente los permisos.

## See Also
Si te interesa profundizar más en temas relacionados, aquí tienes algunos enlaces útiles:
- Documentación oficial de Rust sobre `std::fs`: https://doc.rust-lang.org/std/fs/
- Documentación oficial de Rust sobre `std::path::Path`: https://doc.rust-lang.org/std/path/struct.Path.html
- Tutorial de Rust sobre manejo de errores (incluye operaciones en archivos y directorios): https://doc.rust-lang.org/book/ch09-00-error-handling.html
