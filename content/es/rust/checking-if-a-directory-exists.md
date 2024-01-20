---
title:                "Verificando si un directorio existe"
html_title:           "Javascript: Verificando si un directorio existe"
simple_title:         "Verificando si un directorio existe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Verificar si un directorio existe en Rust es útil cuando necesitas trabajar con archivos. Nos ayuda a evitar errores relacionados con la falta de archivos.

## ¿Cómo se hace?
Primero, importa `std::path::Path`. Luego, usa `Path::new()` para crear un nuevo objeto `Path` y `Path::exists()` para ver si realmente existe el directorio.

```Rust
use std::path::Path; 

fn main() {
    let directorio = Path::new("/ruta/al/directorio");
    if directorio.exists() {
        println!("El directorio existe.");
    } else {
        println!("El directorio no existe.");
    }
}
```

El código imprimirá "El directorio existe." si el directorio está presente o "El directorio no existe." si no lo está.

## Más detallado
Históricamente, la necesidad de verificar si un directorio existe ha existido desde los primeros días de la programación. Cada lenguaje de programación tiene su propia forma de hacerlo.

En Rust, algunas alternativas a Path::exists() serían `fs::metadata()` o `fs::read_dir()`, que retornan un `Result` que puedes usar para comprobar si un directorio existe.

Evita usar `Path::exists()` para "verificar antes de que hagas", porque hacerlo puede introducir carreras de condiciones si algo cambia el sistema de archivos después de que hagas el chequeo. 

## Ver también
1. [Documentación oficial de Rust sobre path::Path](https://doc.rust-lang.org/std/path/struct.Path.html)
2. [¿Cómo verificar si un archivo existe en Rust?](https://stackoverflow.com/questions/26958489/how-to-check-if-a-file-exists-in-rust)