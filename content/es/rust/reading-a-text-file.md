---
title:                "Rust: Leyendo un archivo de texto"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué

¿Estás interesado en aprender Rust y quieres saber cómo leer un archivo de texto en este lenguaje de programación? Entonces sigue leyendo, porque en este artículo te mostraremos cómo hacerlo paso a paso.

## Cómo hacerlo

Para leer un archivo de texto en Rust, primero debes importar la biblioteca `std::fs` y la biblioteca `std::io`. Luego, puedes utilizar la función `fs::read_to_string()` para leer el contenido del archivo y almacenarlo en una variable. A continuación, puedes imprimir el contenido de la variable utilizando la función `println!()`.

```Rust
use std::fs;
use std::io;

fn main() {
    let texto = fs::read_to_string("archivo.txt")
        .expect("No se pudo leer el archivo.");

    println!("{}", texto);
}
```

¡Y eso es todo! Ahora puedes ejecutar tu programa y ver el contenido del archivo de texto en la consola.

## Profundizando

Si quieres comprender mejor cómo funciona el proceso de lectura de un archivo en Rust, aquí hay algunas cosas que debes tener en cuenta:

- La función `fs::read_to_string()` devuelve un resultado `Result` que puede ser `Ok` o `Err`, dependiendo de si la lectura del archivo fue exitosa o no. Por lo tanto, es necesario utilizar la palabra clave `expect()` para manejar los posibles errores.

- La ruta del archivo especificada en la función `read_to_string()` puede ser una ruta absoluta o relativa al directorio donde se encuentra el programa.

- También puedes utilizar la función `fs::read()` si solo quieres leer el archivo como un vector de bytes en lugar de una cadena de texto.

## Ver también

- [Documentación oficial de Rust sobre lectura y escritura de archivos](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial en español de Rust: Introducción a la lectura y escritura de archivos](https://www.elliotherrera.com/tutoriales/introduccion-rust-lectura-escritura-archivos/)
- [Preguntas frecuentes sobre Rust: ¿Cómo leo un archivo?](https://stackoverflow.com/questions/41556300/how-do-i-read-a-file-in-rust)