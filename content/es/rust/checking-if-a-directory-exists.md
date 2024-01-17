---
title:                "Comprobando si existe un directorio"
html_title:           "Rust: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

¡Hola lectores de Rust!

Si eres un programador en Rust, seguramente has encontrado la necesidad de comprobar si un directorio existe en tu código. Esto es una práctica común en la programación y es importante tener un entendimiento claro de cómo hacerlo en Rust.

## ¿Qué y por qué? 

Comprobar si un directorio existe es simplemente verificar si la ruta especificada existe en el sistema de archivos. Esto es importante para los programadores ya que les permite asegurarse de que un directorio específico se encuentra disponible antes de intentar crear archivos en él o acceder a él.

## Cómo hacerlo

En Rust, podemos utilizar el método ```std::fs::metadata(path)```, que devuelve un objeto ```std::fs::Metadata``` que contiene información sobre el archivo o directorio especificado.

```rust
use std::fs;

fn main() {
    let path = "/ruta/a/mi/directorio";

    match fs::metadata(path) {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("¡El directorio existe!");
            } else {
                println!("¡El directorio no existe!");
            }
        }
        Err(_) => println!("¡El directorio no existe!"),
    }
}
```

La función ```is_dir()``` nos permite verificar si el objeto Metadata se refiere a un directorio o no. Además, si queremos asegurarnos de que el directorio existe antes de intentar acceder a él, podemos utilizar el método ```std::fs::metadata(path).is_ok()``` que nos devolverá un booleano indicando si la llamada fue exitosa o no.

## Profundizando

Si bien el uso del método ```std::fs::metadata``` es la forma estándar de comprobar si un directorio existe en Rust, también podemos utilizar la macro ```std::path::Path::exists()``` para obtener un resultado similar. Sin embargo, esta macro solo comprueba si el archivo o directorio existe, sin proporcionar información adicional.

Además, al utilizar el método ```std::fs::metadata```, el comportamiento de la llamada dependerá del sistema operativo en el que se esté ejecutando el código. Por ejemplo, en sistemas Unix, el método devolverá un error si el usuario no tiene permisos suficientes para acceder a la ubicación especificada.

## Ver también

Si aún tienes dudas sobre cómo comprobar si un directorio existe en Rust, puedes consultar la documentación oficial de Rust en [std::fs::metadata](https://doc.rust-lang.org/std/fs/fn.metadata.html) y [std::path::Path](https://doc.rust-lang.org/std/path/struct.Path.html).

¡Espero que este artículo te haya sido útil y que puedas usar esta información para mejorar tus habilidades en Rust! ¡Hasta la próxima!