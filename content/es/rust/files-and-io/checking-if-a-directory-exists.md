---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:29.321030-07:00
description: "C\xF3mo: La biblioteca est\xE1ndar de Rust (`std`) incluye funcionalidad\
  \ para verificar la existencia de un directorio a trav\xE9s de los m\xF3dulos `std::path::Path`\u2026"
lastmod: '2024-03-13T22:44:58.860838-06:00'
model: gpt-4-0125-preview
summary: "La biblioteca est\xE1ndar de Rust (`std`) incluye funcionalidad para verificar\
  \ la existencia de un directorio a trav\xE9s de los m\xF3dulos `std::path::Path`\
  \ y `std::fs`."
title: Comprobando si un directorio existe
weight: 20
---

## Cómo:
La biblioteca estándar de Rust (`std`) incluye funcionalidad para verificar la existencia de un directorio a través de los módulos `std::path::Path` y `std::fs`. Aquí hay un ejemplo simple utilizando el enfoque estándar de Rust:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/ruta/al/directorio");
    if path.exists() && path.is_dir() {
        println!("El directorio existe.");
    } else {
        println!("El directorio no existe.");
    }
}
```

Salida de muestra, asumiendo que el directorio existe:
```
El directorio existe.
```

Para escenarios más complejos o características mejoradas (como operaciones de sistema de archivos asíncronas), podrías considerar usar una biblioteca de terceros como `tokio` con su módulo de sistema de archivos asíncrono (`fs`), especialmente si estás trabajando dentro de un entorno de ejecución asíncrono. Así es cómo podrías lograr lo mismo con `tokio`:

Primero, añade `tokio` a tu `Cargo.toml`:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

Luego, usa `tokio::fs` para verificar si un directorio existe de manera asíncrona:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/ruta/al/directorio";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("El directorio existe.");
            } else {
                println!("La ruta existe pero no es un directorio.");
            }
        },
        Err(_) => println!("El directorio no existe."),
    }
}
```

Salida de muestra, asumiendo que el directorio no existe:
```
El directorio no existe.
```

Estos ejemplos resaltan cómo Rust y su ecosistema ofrecen enfoques tanto sincrónicos como asíncronos para la verificación de existencia de directorios, atendiendo una amplia gama de necesidades de desarrollo de software.
