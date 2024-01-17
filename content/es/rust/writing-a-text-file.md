---
title:                "Creando un archivo de texto"
html_title:           "Rust: Creando un archivo de texto"
simple_title:         "Creando un archivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Escribir un archivo de texto es simplemente crear un documento en el que puedes guardar información textual. Los programadores lo utilizan para almacenar datos importantes o configuraciones en un formato legible para la máquina.

## Cómo:
Los archivos de texto en Rust funcionan a través del módulo `std::fs`, que proporciona funciones para manipular archivos del sistema. A continuación, se muestra un ejemplo de cómo crear y escribir en un archivo de texto:

```Rust
use std::fs::File; // Importar módulo
use std::io::prelude::*; // Importar funciones para manejar Entrada/Salida

fn main() {
	let mut archivo = File::create("mi_archivo.txt").expect("No se pudo crear el archivo");
	// Crea un nuevo archivo con el nombre "mi_archivo.txt"

	archivo.write_all(b"Hola, mundo!").expect("No se pudo escribir en el archivo");
	// Escribe el texto "Hola, mundo!" en el archivo

	println!("Archivo creado y escrito exitosamente");
}
```

Este código creará un archivo llamado "mi_archivo.txt" en la misma carpeta en la que se encuentra el programa y escribirá en él el texto "Hola, mundo!".

## Profundizando:
El uso de archivos de texto en la programación se remonta a los primeros lenguajes de programación y sigue siendo una forma común de almacenar información en la actualidad. Alternativas a los archivos de texto incluyen bases de datos y formatos de archivo más complejos como JSON o XML.

Para escribir en un archivo, es necesario abrirlo en un modo específico (escritura, lectura, ambos) y cerrarlo una vez que se termina de manipularlo. En Rust, esto se hace de forma implícita al usar la función `write_all()`.

## Ver también:
- [The Rust Standard Library](https://doc.rust-lang.org/std/fs/) - Documentación oficial sobre el módulo `std::fs`.
- [Writing and Reading Files in Rust](https://www.educative.io/blog/writing-read-files-rust) - Tutorial sobre cómo escribir y leer archivos en Rust.
- [Rust Programming Language Book](https://doc.rust-lang.org/book/) - Recurso completo para aprender Rust desde cero.