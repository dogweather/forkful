---
title:    "Rust: Comprobando si existe un directorio"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## ¿Por qué comprobar si un directorio existe?

Comprobar si un directorio existe es una tarea importante en la programación de Rust. Puede ser útil en situaciones en las que es necesario asegurarse de que un directorio específico esté presente antes de realizar ciertas operaciones en él, como crear archivos o moverlos. A continuación, exploraremos cómo realizar esta tarea de manera eficiente en Rust.

## Cómo hacerlo

Para verificar si un directorio existe en Rust, podemos utilizar la función `Path::exists()`. Esta función devuelve un booleano que indica si un camino (path) existe o no. Podemos pasar la ruta del directorio que queremos comprobar como argumento para esta función. Luego, podemos usar un condicional para determinar el siguiente paso en nuestro programa.

```rust
use std::path::Path;

let ruta = Path::new("/mi/directorio");

if ruta.exists() {
    println!("El directorio existe.");
} else {
    println!("El directorio no existe.");
}
```

Si el directorio existe, se imprimirá "El directorio existe.". De lo contrario, se imprimirá "El directorio no existe.".

## Profundizando

Al comprobar si un directorio existe, es importante recordar que el resultado de la función `Path::exists()` puede ser afectado por la existencia de enlaces simbólicos. Si la ruta que se está comprobando es un enlace simbólico que apunta a un directorio inexistente, el resultado será "false", incluso si el directorio real existe.

También es importante tener en cuenta que la función `Path::exists()` no comprueba si el usuario actual tiene permiso para acceder al directorio. Si la ruta del directorio existe pero no se tienen los permisos de acceso necesarios, la función devolverá "true" pero se producirá un error al intentar realizar operaciones en él.

## Ver También

- [Documentación de la función `Path::exists()`](https://doc.rust-lang.org/std/path/struct.Path.html#method.exists)
- [Tutorial introductorio a Rust](https://docs.microsoft.com/en-us/learn/modules/rust-get-started/)
- [Ejemplos de código en Rust](https://github.com/emk/CodeSamples/tree/master/Rust)