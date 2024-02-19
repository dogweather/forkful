---
aliases:
- /es/rust/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:32:11.621731-07:00
description: "Calcular una fecha en el futuro o pasado significa encontrar una fecha\
  \ espec\xEDfica antes o despu\xE9s de otra conocida. Los programadores lo hacen\
  \ para\u2026"
lastmod: 2024-02-18 23:09:09.763538
model: gpt-4-1106-preview
summary: "Calcular una fecha en el futuro o pasado significa encontrar una fecha espec\xED\
  fica antes o despu\xE9s de otra conocida. Los programadores lo hacen para\u2026"
title: Calcular una fecha en el futuro o pasado
---

{{< edit_this_page >}}

## Qué y Por Qué?
Calcular una fecha en el futuro o pasado significa encontrar una fecha específica antes o después de otra conocida. Los programadores lo hacen para manejar eventos, plazos, o simplemente para registrar cuánto tiempo ha pasado.

## Cómo Hacerlo:
Para calcular fechas en Rust, puedes usar la crate `chrono`. Primero, agregamos `chrono` a nuestro `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Luego, en nuestro código Rust, podemos sumar o restar días, semanas, etc:

```rust
extern crate chrono;
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("Ahora: {}", now);
    
    let two_weeks = Duration::weeks(2);
    let future_date = now + two_weeks;
    println!("En dos semanas: {}", future_date);

    let thirty_days_ago = Duration::days(-30);
    let past_date = now + thirty_days_ago;
    println!("Hace treinta días: {}", past_date);
}
```

Salida:

```
Ahora: 2023-04-05T15:44:28.601724Z
En dos semanas: 2023-04-19T15:44:28.601724Z
Hace treinta días: 2023-03-06T15:44:28.601724Z
```

## Deep Dive:
Antes de `chrono`, manipular fechas en Rust podía ser tedioso. `chrono` se volvió popular por su facilidad de uso y potencia, influenciado por otras bibliotecas de tiempo en otros lenguajes. Sin embargo, hay alternativas como `time` o incluso funciones de tiempo estándar de Rust para casos más simples.

La implementación interna de `chrono` maneja complejidades como años bisiestos y conversiones de zona horaria. Además, al sumar y restar fechas, es crucial considerar el contexto; por ejemplo, sumar un mes en febrero resulta diferente que en marzo debido a la cantidad de días en cada mes.

## Ver También:
- Documentación de `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- Guía de `chrono` de GitHub: https://github.com/chronotope/chrono
- Librería de tiempo de Rust estándar: https://doc.rust-lang.org/std/time/index.html
