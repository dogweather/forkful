---
title:                "Comparación de dos fechas"
aliases: - /es/rust/comparing-two-dates.md
date:                  2024-01-20T17:33:54.215567-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparación de dos fechas"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Comparar dos fechas es verificar si una es anterior, posterior o la misma que la otra. Los programadores lo hacen para gestionar eventos, expiraciones o cronogramas en sus aplicaciones.

## Cómo hacerlo:

```rust
use chrono::{DateTime, Utc};

fn main() {
    let fecha1 = Utc.ymd(2023, 3, 1).and_hms(12, 0, 0); // 1 de marzo de 2023 a las 12:00
    let fecha2 = Utc.ymd(2023, 4, 1).and_hms(12, 0, 0); // 1 de abril de 2023 a las 12:00

    if fecha1 < fecha2 {
        println!("La primera fecha es anterior a la segunda.");
    } else if fecha1 > fecha2 {
        println!("La primera fecha es posterior a la segunda.");
    } else {
        println!("Las fechas son iguales.");
    }
}
```

Salida esperada:
```
La primera fecha es anterior a la segunda.
```

## Inmersión profunda
En Rust, la tarea de comparar fechas se maneja principalmente por la biblioteca `chrono`, que no es parte de la biblioteca estándar sino una adición de la comunidad. Antes de su creación, los desarrolladores tenían que usar las funciones de tiempo de la biblioteca estándar `std::time`, menos versátiles para manejar fechas.

Existen otros enfoques para comparar fechas, como el uso de la biblioteca `time` o manejar fechas como números (timestamp). Sin embargo, `chrono` ofrece un equilibrio entre facilidad de uso y precisión.

Cuando se comparan dos objetos `DateTime`, `chrono` proporciona métodos que sobrecargan los operadores estándar como `>`, `<` y `==`. Esto permite una sintaxis intuitiva y directa. Además, `chrono` ofrece métodos para sumar o restar duraciones a las fechas, verificar saltos de año bisiesto, y mucho más.

## Ver también
- Documentación oficial de `chrono`: https://docs.rs/chrono/
- Librería estándar de Rust para el tiempo: https://doc.rust-lang.org/std/time/index.html
- Alternativa `time` crate: https://docs.rs/time/
