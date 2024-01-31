---
title:                "Análisis de una fecha a partir de una cadena"
date:                  2024-01-20T15:38:24.831609-07:00
simple_title:         "Análisis de una fecha a partir de una cadena"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Parsear una fecha desde un string convierte el texto en un formato de fecha que Rust puede entender y manipular. Lo hacemos para facilitar operaciones con fechas, como comparaciones y cálculos.

## Cómo hacerlo:
Vamos a usar `chrono`, una biblioteca popular para manejar fechas y horas en Rust.

Añade `chrono` a tu `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4.19"
```

A continuación, el código para parsear una fecha:

```rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let fecha_texto = "2023-04-12";
    let formato = "%Y-%m-%d";
    let fecha_parseada = NaiveDate::parse_from_str(fecha_texto, formato).unwrap();

    println!("Fecha parseada: {}", fecha_parseada);
}
```

Output:

```
Fecha parseada: 2023-04-12
```

## Inmersión Profunda
Parsear fechas es fundamental desde los albores de la programación. `chrono` ha simplificado mucho el proceso en Rust, comparado con las herramientas de fecha y hora en C++ (`<ctime>`) por ejemplo. Alternativamente, puedes usar la biblioteca estándar `time`, pero `chrono` ofrece más funcionalidades y es ampliamente usada en la comunidad Rust. El parseo implica convertir strings en tipos de datos de fecha, lo que puede incluir manejo de zonas horarias y formatos personalizados.

## Ver También
- Documentación oficial de `chrono`: [https://docs.rs/chrono](https://docs.rs/chrono)
- Documentación de Rust `std::time` Module: [https://doc.rust-lang.org/stable/std/time/](https://doc.rust-lang.org/stable/std/time/)
