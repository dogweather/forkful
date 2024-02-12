---
title:                "Trabajando con JSON"
aliases:
- /es/rust/working-with-json/
date:                  2024-02-03T19:24:00.698350-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabajando con JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Trabajar con JSON (JavaScript Object Notation) en Rust consiste en analizar datos JSON para convertirlos en estructuras de datos de Rust y serializar estructuras de datos de Rust de vuelta a JSON. Los programadores lo hacen para interactuar con APIs web, archivos de configuración o cualquier formato de intercambio de datos donde se use JSON debido a su formato ligero y legible por humanos.

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
