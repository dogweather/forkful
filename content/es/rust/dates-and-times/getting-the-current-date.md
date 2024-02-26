---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:39.282115-07:00
description: "Recuperar la fecha actual en Rust es una tarea com\xFAn para acciones\
  \ como registrar, operaciones basadas en tiempo o simplemente mostrar la fecha.\
  \ A\u2026"
lastmod: '2024-02-25T18:49:55.351107-07:00'
model: gpt-4-0125-preview
summary: "Recuperar la fecha actual en Rust es una tarea com\xFAn para acciones como\
  \ registrar, operaciones basadas en tiempo o simplemente mostrar la fecha. A\u2026"
title: Obteniendo la fecha actual
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Recuperar la fecha actual en Rust es una tarea común para acciones como registrar, operaciones basadas en tiempo o simplemente mostrar la fecha. A diferencia de algunos lenguajes que incluyen la funcionalidad de fecha y hora en su biblioteca estándar, Rust fomenta el uso de una biblioteca de terceros robusta, chrono, para una manipulación de fecha y hora exhaustiva debido a su funcionalidad superior y facilidad de uso.

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
