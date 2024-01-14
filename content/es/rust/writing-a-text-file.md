---
title:    "Rust: Redactando un archivo de texto"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto en Rust?

Escribir un archivo de texto puede ser una tarea común en el desarrollo de software, especialmente cuando se trata de almacenar datos de forma persistente o crear registros de seguimiento. En Rust, esto se puede lograr de manera eficiente y segura gracias a su fuerte enfoque en la concurrencia y la gestión de recursos.

## Cómo hacerlo

Para escribir un archivo de texto en Rust, primero debemos importar el módulo "std::fs" (filesystem), que nos proporcionará las funciones necesarias para trabajar con archivos. Luego, usaremos la función "create" para crear un nuevo archivo y la función "write" para escribir en él. Por ejemplo:

```Rust
use std::fs;

// Crear un archivo llamado "texto.txt" en el directorio actual
let archivo = fs::File::create("texto.txt")?;

// Escribir contenido en el archivo
let contenido = "Este es un ejemplo de texto escrito en Rust";
archivo.write(contenido.as_bytes())?;
```

Una vez que ejecutemos este código, se creará un archivo llamado "texto.txt" en el directorio en el que se encuentra nuestro programa, y el contenido que hemos especificado se escribirá en él. Tenga en cuenta que se utiliza el método ".as_bytes()" para convertir el texto en una secuencia de bytes, ya que es así como los datos se almacenan en un archivo de texto.

## Profundizando

Hay varias cosas que debemos tener en cuenta al escribir un archivo de texto en Rust:

- El uso de la función "write" sobrescribirá cualquier contenido preexistente en el archivo. Para agregar contenido adicional, podemos utilizar la función "write_all" en su lugar.

- Es importante manejar los errores correctamente al escribir en un archivo. Por eso, utilizamos el signo de interrogación ("?") al final de cada línea de código, lo que indica que queremos que se manejen los errores de manera adecuada en caso de que ocurran.

- También podemos utilizar la función "flush" después de escribir en un archivo para asegurarnos de que todos los datos se hayan escrito correctamente antes de continuar con nuestro programa.

## Ver también

- [Documentación de "std::fs" en el sitio oficial de Rust](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial de Rust: manejo de archivos](https://www.tutorialspoint.com/rust/rust_file_io.htm)
- [Escribir y leer archivos en Rust](https://dev.to/dsleandro/writing-and-reading-files-in-rust-1m6d)