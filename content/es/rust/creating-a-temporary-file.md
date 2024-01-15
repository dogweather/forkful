---
title:                "Creando un archivo temporal"
html_title:           "Rust: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por Qué

¿Alguna vez has necesitado crear un archivo temporal en tu programa? Ya sea para almacenar datos temporales o para realizar alguna operación específica, la creación de archivos temporales es una tarea común en la programación. En este artículo, vamos a explorar cómo crear archivos temporales en Rust y por qué es útil hacerlo.

## Cómo

Primero, importaremos el módulo `std::fs` para poder interactuar con el sistema de archivos en Rust. Luego, usaremos la función `tempfile` que nos permite crear un archivo temporal en el directorio actual:

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
  let temp_file = tempfile::tempfile().expect("Error al crear archivo temporal.");
}
```

Ahora que hemos creado nuestro archivo temporal, podemos escribir en él utilizando los métodos de `std::io::Write`:

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
  let mut temp_file = tempfile::tempfile().expect("Error al crear archivo temporal.");
  temp_file.write(b"¡Hola desde Rust!").expect("Error al escribir en archivo temporal.");
}
```

Por último, debemos asegurarnos de cerrar el archivo temporal después de haber terminado de usarlo para liberar recursos:

```Rust
use std::fs::File;
use std::io::Write;

fn main() {
  let mut temp_file = tempfile::tempfile().expect("Error al crear archivo temporal.");
  temp_file.write(b"¡Hola desde Rust!").expect("Error al escribir en archivo temporal.");

  // Cerramos el archivo temporal
  temp_file.close().expect("Error al cerrar archivo temporal.");
}
```

## Deep Dive

La función `tempfile` en realidad devuelve un objeto `NamedTempFile` que implementa el trait `Write`. Esto significa que podemos utilizar todos los métodos de `Write` para escribir en nuestro archivo temporal, como por ejemplo `write_all` o `flush`. Además, al utilizar `tempfile` no tenemos que preocuparnos por darle un nombre único al archivo, ya que se encarga automáticamente de eso.

También hay casos en los que necesitamos mantener el archivo temporal después de cerrarlo, por ejemplo, si queremos que nuestros datos sean accesibles desde otros procesos. En ese caso, podemos utilizar el método `persist` en lugar de `close` para mantener el archivo abierto en el sistema de archivos.

## Ver También

- [Documentación oficial de Rust sobre creación de archivos temporales](https://doc.rust-lang.org/std/fs/struct.NamedTempFile.html)
- [Ejemplo de creación de archivo temporal en Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=08ae147963a3079467bf50a092bf25dc)