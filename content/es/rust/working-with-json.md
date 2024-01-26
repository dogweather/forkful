---
title:                "Trabajando con JSON"
html_title:           "Bash: Trabajando con JSON"
simple_title:         "Trabajando con JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-json.md"
---

{{< edit_this_page >}}

## ¿Qué es y Por Qué?

Trabajar con JSON significa manejar un formato de texto ligero para intercambiar datos. Los programadores lo usan por su facilidad de lectura y porque es muy común en APIs y configuraciones.

## Cómo hacerlo:

Para manejar JSON en Rust, usamos la crate `serde_json`. Primero, inclúyela en tu `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

Aquí un ejemplo básico de cómo serializar y deserializar:

```rust
use serde::{Serialize, Deserialize};
use serde_json::{Result, Value};

#[derive(Serialize, Deserialize)]
struct Usuario {
    nombre: String,
    edad: u8,
}

fn main() -> Result<()> {
    // Serializar
    let usuario = Usuario {
        nombre: "Alicia".to_string(),
        edad: 30,
    };
    let usuario_json = serde_json::to_string(&usuario)?;
    println!("{}", usuario_json);

    // Deserializar
    let datos_json = r#"{"nombre":"Alicia","edad":30}"#;
    let u: Usuario = serde_json::from_str(datos_json)?;
    println!("{} tiene {} años.", u.nombre, u.edad);

    Ok(())
}
```

Salida esperada:

```
{"nombre":"Alicia","edad":30}
Alicia tiene 30 años.
```

## Conociendo Más

JSON, acrónimo de JavaScript Object Notation, fue propuesto por Douglas Crockford en los 2000s. Otras alternativas incluyen XML y YAML, pero JSON prevalece por su simpleza. Cuando usas `serde_json` en Rust, aprovechas un proceso llamado serialización que convierte estructuras de datos a JSON y viceversa.

## Ver También

- La documentación oficial de `serde_json`: https://docs.serde.rs/serde_json/
- Guía de Serde sobre cómo trabajar con JSON en Rust: https://serde.rs/json.html
- Repositorio de Rust `serde_json` en GitHub: https://github.com/serde-rs/json
