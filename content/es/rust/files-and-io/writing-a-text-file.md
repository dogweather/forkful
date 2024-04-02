---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:08.802729-07:00
description: "Escribir un archivo de texto en Rust implica crear, escribir y, potencialmente,\
  \ a\xF1adir datos a un archivo en el sistema de archivos. Los programadores\u2026"
lastmod: '2024-03-13T22:44:58.864891-06:00'
model: gpt-4-0125-preview
summary: "Escribir un archivo de texto en Rust implica crear, escribir y, potencialmente,\
  \ a\xF1adir datos a un archivo en el sistema de archivos. Los programadores\u2026"
title: Escribiendo un archivo de texto
weight: 24
---

## Qué & Por qué?
Escribir un archivo de texto en Rust implica crear, escribir y, potencialmente, añadir datos a un archivo en el sistema de archivos. Los programadores realizan esta operación para persistir datos, como registros de aplicaciones, configuración o contenido generado por usuarios, asegurando la durabilidad de los datos más allá del alcance de la ejecución del programa.

## Cómo hacerlo:
La biblioteca estándar de Rust proporciona herramientas robustas para la manipulación de archivos, encapsuladas principalmente dentro de los módulos `std::fs` y `std::io`. Aquí hay un ejemplo básico para crear y escribir en un archivo de texto:

```rust
use std::fs::File;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = File::create("hello.txt")?;
    file.write_all(b"Hello, world!")?;
    Ok(())
}
```

Después de ejecutar este código, encontrarás un archivo llamado `hello.txt` con el contenido "Hello, world!".

Para escenarios más complejos, como añadir a un archivo o manejar datos más grandes de manera eficiente, Rust ofrece funcionalidades adicionales. Aquí te mostramos cómo añadir texto a un archivo existente:

```rust
use std::fs::OpenOptions;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut file = OpenOptions::new()
        .write(true)
        .append(true)
        .open("hello.txt")?;
        
    file.write_all(b" Añadiendo más texto.")?;
    Ok(())
}
```

Ejecutar esto añadirá " Añadiendo más texto." al final de `hello.txt`.

En algunos casos, aprovechar las bibliotecas de terceros puede simplificar las operaciones con archivos. La crate `serde`, combinada con `serde_json`, por ejemplo, permite serializar y deserializar estructuras de datos desde y hacia el formato JSON, ofreciendo un enfoque de alto nivel para la escritura de archivos:

```rust
use serde::{Serialize, Deserialize};
use serde_json;
use std::fs::File;

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
}

fn main() -> std::io::Result<()> {
    let user = User { id: 1, name: "Jane Doe".into() };
    let file = File::create("user.json")?;
    serde_json::to_writer(file, &user)?;
    Ok(())
}
```

Después de ejecutar el código anterior, `user.json` contendrá una representación JSON de la estructura `User`. Ten en cuenta que usar `serde` y `serde_json` requiere agregar estas crates a tu `Cargo.toml`.

Escribir archivos de texto en Rust, ya sea a través de la biblioteca estándar o con la ayuda de crates externas, es una manera directa pero potente de gestionar la persistencia de datos en tus aplicaciones.
