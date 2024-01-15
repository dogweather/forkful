---
title:                "Escribiendo un archivo de texto"
html_title:           "Rust: Escribiendo un archivo de texto"
simple_title:         "Escribiendo un archivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir un archivo de texto?

Escribir un archivo de texto puede ser útil en varias situaciones, como por ejemplo guardar datos importantes, crear un archivo de configuración o simplemente dejar una nota para uno mismo o para alguien más.

## Cómo hacerlo

Para escribir un archivo de texto en Rust, utilizamos el módulo de manejo de archivos llamado `std::fs`. Primero, debemos importarlo añadiendo `use std::fs;` al principio de nuestro código. Luego, podemos utilizar su método `write` para crear el archivo y escribir en él. A continuación, se muestra un ejemplo de cómo escribir "Hola Mundo" en un archivo de texto llamado `saludo.txt`:

```Rust
use std::fs; // importar el módulo de manejo de archivos

fn main() {
    // Escribir en el archivo
    fs::write("saludo.txt", "Hola Mundo").expect("No se pudo escribir en el archivo");
}
```

Si el archivo ya existe, el método `write` sobrescribirá su contenido. Si queremos agregar texto al final del archivo, podemos utilizar el método `append` en su lugar. Además, podemos utilizar la macro `format!` para incluir variables o expresiones en nuestro texto. A continuación, se muestra un ejemplo de cómo agregar un nombre al saludo anterior:

```Rust
use std::fs; // importar el módulo de manejo de archivos

fn main() {
    let nombre = "Pedro";

    // Agregar nombre al saludo
    let saludo = format!("Hola {}", nombre);

    // Escribir en el archivo
    fs::append("saludo.txt", saludo).expect("No se pudo escribir en el archivo");
}
```

Una vez que hayamos escrito en nuestro archivo, podemos comprobar su contenido abriendo el archivo en un editor de texto o utilizando el método `read_to_string` para leer su contenido y mostrarlo por pantalla. También podemos utilizar el método `metadata` para obtener información sobre el archivo, como su tamaño, fecha de creación, etc.

## Profundizando

Además de escribir en un archivo de texto, también podemos crearlo, eliminarlo, renombrarlo y moverlo utilizando los métodos proporcionados por el módulo `std::fs`. También es importante tener en cuenta que, al igual que con cualquier operación de entrada/salida, es recomendable manejar los posibles errores que puedan surgir al escribir en un archivo.

## Ver también

- [La documentación oficial de Rust sobre manejo de archivos](https://doc.rust-lang.org/std/fs)
- [Un tutorial sobre escritura de archivos en Rust](https://www.rust-lang.org/learn/get-started#working-with-files)
- [Un ejemplo de proyecto en GitHub que utiliza escritura de archivos en Rust](https://github.com/PistonDevelopers/hematite_fileserver/blob/master/src/main.rs)