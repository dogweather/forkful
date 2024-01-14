---
title:    "Rust: Leyendo un archivo de texto"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## ¿Por qué leer un archivo de texto en Rust?

Leer y manipular archivos de texto es una tarea común en la programación. Ya sea para leer datos de un archivo de configuración o para procesar datos almacenados en un archivo, es importante saber cómo realizar esta operación en Rust. En este artículo, aprenderemos cómo leer un archivo de texto en Rust y veremos algunas funciones útiles para trabajar con él.

## Cómo hacerlo

Para leer un archivo de texto en Rust, primero debemos abrirlo usando la función `std::fs::File::open` y pasarle el nombre del archivo como parámetro. Luego, usaremos el método `read_to_string` para almacenar el contenido del archivo en una variable como una cadena de texto.

```rust
use std::fs::File;
use std::io::Read;

fn main() {
    // Abrimos el archivo
    let mut archivo = File::open("ejemplo.txt").expect("No se pudo abrir el archivo");

    // Creamos una variable para almacenar el contenido
    let mut contenido = String::new();

    // Leemos el archivo y almacenamos el contenido en la variable
    archivo.read_to_string(&mut contenido).expect("No se pudo leer el archivo");

    // Imprimimos el contenido del archivo
    println!("{}", contenido);
}
```

Si el archivo contiene texto, veremos su contenido impreso en la consola. Sin embargo, si intentamos imprimir el contenido de un archivo binario, obtendremos un error ya que el método `read_to_string` solo funciona con archivos que contienen texto.

## Sumérgete en el mundo de la lectura de archivos en Rust

Además de leer y almacenar el contenido de un archivo, podemos realizar otras operaciones útiles en él. Por ejemplo, podemos obtener información sobre el archivo, como su tamaño, usando el método `metadata` y el tipo de dato `std::fs::Metadata`.

También podemos leer el archivo línea por línea usando el método `lines` y el tipo de dato `std::io::BufRead`. Esto es útil cuando queremos procesar cada línea individualmente en lugar de todo el contenido del archivo.

Otra función interesante es el método `seek`, el cual nos permite buscar una ubicación específica en el archivo y leer o escribir a partir de ese punto.

En resumen, hay muchas funciones y métodos útiles para trabajar con archivos en Rust. Deberías explorarlos y probar su funcionamiento para familiarizarte con ellos.

## Ver también

- [Documentación de Rust sobre lectura de archivos](https://doc.rust-lang.org/std/fs/struct.File.html#method.read_to_string)
- [Código de ejemplo de lectura de archivos en Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1fd2deb68b110d83d2473044a43ac9f7)