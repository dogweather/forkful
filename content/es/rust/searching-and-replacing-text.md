---
title:    "Rust: Buscando y reemplazando texto"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por qué

La búsqueda y reemplazo de texto es una tarea común en la programación, especialmente cuando se trabaja con grandes cantidades de código. Usar un lenguaje de programación como Rust puede simplificar y mejorar esta tarea gracias a su enfoque en la seguridad y concurrencia.

## Cómo hacerlo

Para realizar la búsqueda y reemplazo de texto en Rust, se pueden seguir estos pasos:

1. Importar la librería `std::fs` que permite trabajar con archivos.

2. Abrir el archivo que se desea modificar utilizando la función `File::open()` y almacenar el resultado en una variable.

3. Leer el contenido del archivo con la función `read_to_string()` y guardarlo en una variable de tipo `String`.

4. Utilizar la función `replace()` en la variable de tipo `String` con los parámetros que se deseen reemplazar.

5. Escribir el contenido modificado en el archivo utilizando la función `write_all()`.

A continuación, un ejemplo de código para reemplazar todas las apariciones de "hola" por "hello" en un archivo de texto:

````Rust
use std::fs::File;
use std::io::{Read, Write};

fn main() {
    // Abrir el archivo y almacenar el resultado en una variable
    let mut archivo = File::open("texto.txt").expect("No se puede abrir el archivo");

    // Leer el contenido del archivo y guardarlo en una variable de tipo String
    let mut contenido = String::new();
    archivo.read_to_string(&mut contenido).expect("No se puede leer el archivo");

    // Utilizar la función replace() para reemplazar las apariciones de "hola" por "hello"
    let contenido_modificado = contenido.replace("hola", "hello");

    // Escribir el contenido modificado en el archivo
    let mut archivo_modificado =
        File::create("texto_modificado.txt").expect("No se puede crear el archivo");
    archivo_modificado
        .write_all(contenido_modificado.as_bytes())
        .expect("No se puede escribir en el archivo");
}
````

El resultado del código anterior sería un archivo llamado "texto_modificado.txt" con todas las apariciones de "hola" reemplazadas por "hello".

## Profundizando

Además de la función `replace()`, Rust también ofrece otras opciones para realizar la búsqueda y reemplazo de texto. Algunas de ellas son:

- `replace_range()` para reemplazar una sección específica del texto.

- `split()` y `join()` para dividir y unir el texto utilizando un delimitador.

- Expresiones regulares con la librería `regex`, que permiten buscar y reemplazar patrones específicos en el texto.

Explorar estas opciones y su implementación en código puede ayudar a mejorar el proceso de búsqueda y reemplazo de texto en Rust.

## Ver también

- Documentación oficial de Rust sobre [la librería `std::fs`](https://doc.rust-lang.org/std/fs/index.html)

- Documentación oficial de Rust sobre [la librería `regex`](https://docs.rs/regex/)

- [Ejemplos y ejercicios prácticos](https://rust-lang-nursery.github.io/rust-cookbook/text/replacing.html) de búsqueda y reemplazo de texto en Rust.