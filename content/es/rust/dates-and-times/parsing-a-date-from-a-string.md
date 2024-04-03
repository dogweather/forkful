---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:13.071685-07:00
description: "C\xF3mo hacerlo: #."
lastmod: '2024-03-13T22:44:58.856000-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Analizando una fecha a partir de una cadena de texto
weight: 30
---

## Cómo hacerlo:


### Usando la Biblioteca Estándar de Rust (`chrono` Crate)
La biblioteca estándar de Rust no incluye directamente el análisis de fechas, pero el crate `chrono`, ampliamente utilizado, es una solución robusta para la manipulación de fechas y horas. Primero, añade `chrono` a tu `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Luego, utiliza `chrono` para analizar una cadena de fecha en un objeto `NaiveDate`:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Falló al analizar la fecha");

    println!("Fecha analizada: {}", date);
}

// Salida de Ejemplo:
// Fecha analizada: 2023-04-01
```

### Usando el Manejo Avanzado de Fecha y Hora de Rust (`time` Crate)
Para un manejo más avanzado de fecha y hora, incluyendo un análisis más ergonómico, considera el crate `time`. Primero, inclúyelo en tu `Cargo.toml`:

```toml
[dependencies]
time = "0.3"
```

Luego, analiza una cadena de fecha usando el tipo `Date` y `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Falló al analizar fecha y hora");

    println!("Fecha y hora analizadas: {}", parsed_date);
}

// Salida de Ejemplo:
// Fecha y hora analizadas: 2023-04-01 12:34:56
```

Ambos ejemplos muestran cómo Rust, con la ayuda de crates de terceros, facilita el análisis de cadenas de fecha en objetos de fecha manipulables, convirtiéndolo en una herramienta poderosa para el desarrollo de software que involucra datos temporales.
