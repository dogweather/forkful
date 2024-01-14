---
title:    "Rust: Comprobando si existe un directorio"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

¡Hola a todos! En esta publicación, vamos a hablar sobre cómo verificar si un directorio existe en Rust, un lenguaje de programación moderno y seguro. Si eres nuevo en Rust y te estás preguntando por qué querrías hacer esto, sigue leyendo para descubrirlo.

## ¿Por qué?

Antes de sumergirnos en el código, hablemos rápidamente de por qué querrías verificar si un directorio existe en Rust. En la programación, a menudo necesitamos acceder o manipular archivos o directorios en el sistema operativo. Y para hacer eso, primero necesitamos asegurarnos de que el directorio que estamos buscando realmente exista. De lo contrario, podríamos encontrarnos con problemas o errores inesperados. Así que, verificar si un directorio existe es una práctica común que nos ayuda a evitar posibles problemas en nuestro código.

## Cómo hacerlo

Ahora veamos cómo podemos verificar si un directorio existe en Rust. Primero, necesitamos importar la librería `std::fs`, que nos proporciona funciones para interactuar con el sistema de archivos. Luego, usaremos la función `metadata()` para obtener información sobre un archivo o directorio en particular. Esta función nos devolverá un `std::fs::Metadata` que contiene información como el tamaño, permisos y otras propiedades del archivo o directorio.

Aquí hay un ejemplo de código que comprueba si un directorio llamado "docs" existe en el directorio actual:

```rust
use std::fs; // importamos la librería std::fs

fn main() {
    if let Ok(metadata) = fs::metadata("docs") { // utilizamos fs::metadata para obtener información sobre el directorio "docs"
        if metadata.is_dir() { // verificamos si es un directorio
            println!("¡El directorio existe!"); // ¡directorio existe!
        } else {
            println!("¡No es un directorio!"); // no es un directorio
        }
    } else {
        println!("¡El directorio no existe!"); // el directorio no existe
    }
}
```

Si el directorio "docs" existe, el programa imprimirá "¡El directorio existe!". De lo contrario, imprimirá "¡El directorio no existe!".

## Deep Dive

Para aquellos que quieran profundizar más, aquí hay algunas cosas adicionales sobre la verificación de directorios en Rust. También podemos usar la función `exists()` de la librería `std::fs` para verificar si un directorio existe. Esta función nos devolverá un `bool` que indica si el archivo o directorio existe o no.

Además, si necesitamos crear un directorio si no existe, podemos usar la función `create_dir()` o `create_dir_all()` según sea necesario. Estas funciones crearán el directorio especificado si aún no existe.

## Ver también

Esperamos que hayas encontrado útil esta explicación sobre cómo verificar si un directorio existe en Rust. Para obtener más información sobre el lenguaje Rust, aquí hay algunos recursos adicionales en español:

- [Documentación oficial de Rust en español](https://doc.rust-lang.org/book/index.html#documentacion-oficial)
- [Comunidad de programación en Rust en español](https://github.com/rust-lang-es/proyecto/wiki)
- [¡Aprende Rust mientras construyes cosas divertidas!](https://www.youtube.com/watch?v=raLFl8dibEA&list=PL7wieblA16EwXQvR-iFdbWRInjHTp1Z4e) (en español)

¡Hasta la próxima!