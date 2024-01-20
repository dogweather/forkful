---
title:                "Leyendo un archivo de texto"
html_title:           "Arduino: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué? 

Leer un archivo de texto refiere a la acción de obtener información de un archivo de texto almacenado en cualquier dispositivo de almacenamiento. Los programadores lo hacen para poder manipular o utilizar los datos almacenados de forma efectiva y eficiente.

## Cómo hacerlo:

El siguiente código en Rust muestra cómo leer un archivo de texto.

```Rust
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn main() {
    let file = File::open("example.txt").expect("No se puede abrir el archivo");
    let reader = BufReader::new(file);

    for linea in reader.lines() {
        let linea = linea.expect("No se puede leer la línea");
        println!("{}", linea);
    }
}
```

Si tienes en tu disco un archivo llamado `example.txt` con el contenido "Hola mundo", la salida esperada será:

```Rust
Hola mundo
```

## Análisis detallado

(1) En términos históricos, la lectura de archivos es uno de los fundamentos de la programación, facilitando el acceso y la manipulación de datos almacenados.

(2) Como alternativa, podrías querer mantener tus datos en una base de datos. Pero usar archivos de texto es útil para datos más simples y es ideal para la persistencia de datos con estructuras poco complejas.

(3) Rust implementa la lectura de archivos utilizando la abstracción de un `File`, que se envuelve en un `BufReader` para proporcionar capacidades avanzadas de lectura, al leer eficientemente líneas de un archivo.

## Ver también

1. Documentación oficial de Rust para la lectura de archivos: https://doc.rust-lang.org/stable/rust-by-example/std_misc/file/open.html
2. Tutorial sobre operaciones de archivo en Rust: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-io-idioms.html
3. Resultados y manejo de errores detallados en Rust: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html