---
title:                "Manejo de errores"
date:                  2024-01-26T00:56:59.700648-07:00
model:                 gpt-4-1106-preview
simple_title:         "Manejo de errores"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/handling-errors.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

El manejo de errores es lidiar con las cosas cuando se tuercen. Los programadores lo hacen para manejar lo inesperado, asegurando que sus programas en Rust sean robustos y no simplemente se bloqueen cuando se encuentran con un contratiempo.

## Cómo hacerlo:

Rust maneja los errores de dos maneras principales: errores recuperables e irrecuperables. Vamos a ver ambos.

Los errores recuperables utilizan `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Archivo abierto exitosamente."),
        Err(_e) => println!("No se pudo abrir el archivo."),
    }
}
```

La salida podría ser "Archivo abierto exitosamente." o "No se pudo abrir el archivo." dependiendo de tu `hello.txt`.

Para errores irrecuperables, usamos `panic!`:

```Rust
fn main() {
    // Esto causará que el programa entre en pánico porque probablemente el archivo no exista.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Ejecútalo y verás un mensaje de pánico. Tu programa se detiene en seco.

## Inmersión Profunda

Históricamente, el manejo de errores en programación ha sido un lío. Rust lo hace bien con una clara distinción entre errores recuperables e irrecuperables.

El enum `Result` es para errores recuperables. Es explícito: manejas la variante `Ok` o `Err`. También tienes métodos como `unwrap()` y `expect()`, pero son atajos rápidos y sucios que pueden llevar a un `panic!`.

`panic!` es la forma en que Rust grita que algo realmente malo ha ocurrido y no puede lidiar con ello. Es como un error irrecuperable que detiene la ejecución de inmediato. Un pánico en Rust a menudo se siente con errores que no esperas manejar, como indizar fuera de los límites de un arreglo.

El manejo de errores devolviendo un `Result` se prefiere cuando esperas lidiar con errores. Es el Rust idiomático, lo que significa que es la forma en que los desarrolladores de Rust acordaron hacer las cosas. También está `Option<T>`, para casos en que un error es simplemente algo que es `None` en lugar de `Some(T)`. Se trata de esperar lo inesperado sin miedo.

¿Alternativas? Claro, podrías usar otros crates de manejo de errores para más funciones o uso ergonómico. Como `anyhow` para manejo de errores simple, o `thiserror` para errores en código de biblioteca.

## Ver También

¿Interesado en profundizar más? Aquí es donde ir:

- [Libro de Rust sobre Manejo de Errores](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Un gran lugar para entender la filosofía de manejo de errores de Rust.
- [Rust por Ejemplo: Manejo de Errores](https://doc.rust-lang.org/rust-by-example/error.html) - Ejemplos interactivos para ensuciarte las manos.

Recuerda, un buen manejo de errores no es solo codificar; es cuidar a los usuarios de tu código. ¡Feliz codificación!
