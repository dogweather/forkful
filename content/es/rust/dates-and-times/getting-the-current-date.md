---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:39.282115-07:00
description: "C\xF3mo hacerlo: #."
lastmod: '2024-03-13T22:44:58.857096-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Obteniendo la fecha actual
weight: 29
---

## Cómo hacerlo:


### Usando la Biblioteca Estándar de Rust
La biblioteca estándar de Rust ofrece una forma limitada pero rápida de obtener la hora actual, aunque no directamente la fecha actual en un formato de calendario. Así es cómo hacerlo:

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("Hora actual: {} segundos desde la Época Unix.", n.as_secs()),
        Err(_) => panic!("¡SystemTime antes de la Época Unix!"),
    }
}
```

Salida:
```
Hora actual: 1615390665 segundos desde la Época Unix.
```

### Usando la Biblioteca Chrono
Para una funcionalidad de fecha y hora más completa, incluyendo obtener la fecha actual, deberías usar la biblioteca `chrono`. Primero, añade `chrono` a tu `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Luego, puedes usar `chrono` para obtener la fecha actual:

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let ahora = Local::now();
    println!("Fecha actual: {}-{}-{}", ahora.year(), ahora.month(), ahora.day());
}
```

Salida:
```
Fecha actual: 2023-4-20
```

La biblioteca `chrono` facilita el trabajo con fechas y horas, ofreciendo una amplia gama de funcionalidades más allá de solo recuperar la fecha actual, incluyendo el análisis, la formateación y operaciones aritméticas sobre fechas y horas.
