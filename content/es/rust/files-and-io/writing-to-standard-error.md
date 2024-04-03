---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:26.755889-07:00
description: "C\xF3mo hacerlo: Rust proporciona una manera sencilla de escribir en\
  \ stderr utilizando la macro `eprintln!`, similar a c\xF3mo `println!` se usa para\
  \ stdout.\u2026"
lastmod: '2024-03-13T22:44:58.862868-06:00'
model: gpt-4-0125-preview
summary: "Rust proporciona una manera sencilla de escribir en stderr utilizando la\
  \ macro `eprintln!`, similar a c\xF3mo `println!` se usa para stdout."
title: "Escribiendo en el error est\xE1ndar"
weight: 25
---

## Cómo hacerlo:
Rust proporciona una manera sencilla de escribir en stderr utilizando la macro `eprintln!`, similar a cómo `println!` se usa para stdout. Aquí hay un ejemplo básico:

```rust
fn main() {
    eprintln!("¡Este es un mensaje de error!");
}
```

Salida de muestra (a error estándar):
```
¡Este es un mensaje de error!
```

Para tener más control sobre los mensajes de error, como cuando quieres formatear texto o manejar resultados de E/S, usa la función `stderr` del módulo `std::io`. Este método proporciona un manejador al flujo global de stderr, al cual puedes escribir usando métodos como `write_all` o `writeln` del trait `Write`:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Mensaje de error formateado: {}", 404).expect("Fallo al escribir en stderr");
}
```

Salida de muestra (a error estándar):
```
Mensaje de error formateado: 404
```

Si estás trabajando en entornos o aplicaciones donde dependes de bibliotecas para el registro o manejo de errores, bibliotecas como `log` y `env_logger` son populares. Aunque se utilizan más para propósitos de registro, son configurables y pueden dirigir niveles de error de registro a stderr. A continuación, se muestra un ejemplo de uso simple utilizando `log` y `env_logger`:

Primero, agrega las dependencias a tu `Cargo.toml`:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Luego, configura y utiliza el registro en tu aplicación:
```rust
fn main() {
    env_logger::init();
    log::error!("Este es un mensaje de error registrado en stderr");
}
```

Ejecutar este programa (después de configurar `env_logger` con una variable de entorno apropiada, por ejemplo, `RUST_LOG=error`) producirá el mensaje de error en stderr, utilizando la infraestructura de registro.

```plaintext
ERROR: Este es un mensaje de error registrado en stderr
```
