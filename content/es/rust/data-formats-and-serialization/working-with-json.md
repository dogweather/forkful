---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:00.698350-07:00
description: "C\xF3mo hacerlo: Para trabajar con JSON en Rust, se utiliza ampliamente\
  \ el crate `serde` junto con `serde_json` para la serializaci\xF3n y deserializaci\xF3\
  n.\u2026"
lastmod: '2024-03-13T22:44:58.868162-06:00'
model: gpt-4-0125-preview
summary: "Para trabajar con JSON en Rust, se utiliza ampliamente el crate `serde`\
  \ junto con `serde_json` para la serializaci\xF3n y deserializaci\xF3n."
title: Trabajando con JSON
weight: 38
---

## Cómo hacerlo:
Para trabajar con JSON en Rust, se utiliza ampliamente el crate `serde` junto con `serde_json` para la serialización y deserialización. Primero, asegúrate de incluir estos en tu `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Ejemplo 1: Deserializar JSON a una Estructura de Rust
Define una estructura de Rust y usa las macros derivadas para `Deserialize` y `Serialize`:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct Usuario {
    id: u32,
    nombre: String,
    email: String,
}

fn main() {
    let datos_json = r#"
        {
            "id": 1,
            "nombre": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let usuario: Usuario = serde_json::from_str(datos_json).unwrap();

    println!("ID del Usuario: {}", usuario.id);
    println!("Nombre del Usuario: {}", usuario.nombre);
    println!("Correo Electrónico del Usuario: {}", usuario.email);
}
```

**Salida:**

```
ID del Usuario: 1
Nombre del Usuario: Jane Doe
Correo Electrónico del Usuario: jane.doe@example.com
```

### Ejemplo 2: Serializar una Estructura de Rust a JSON
Usando la misma estructura `Usuario`:

```rust
let usuario = Usuario {
    id: 1,
    nombre: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let datos_json = serde_json::to_string(&usuario).unwrap();

println!("{}", datos_json);
```

**Salida:**

```json
{"id":1,"nombre":"Jane Doe","email":"jane.doe@example.com"}
```

Estos ejemplos demuestran el flujo básico de deserialización de JSON en estructuras de Rust y la serialización de estructuras de Rust de vuelta a cadenas JSON. Serde proporciona un rico conjunto de herramientas para trabajar con JSON, incluyendo el manejo de campos opcionales, anidamientos complejos y tipos no directamente soportados por JSON.
