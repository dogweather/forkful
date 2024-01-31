---
title:                "Obteniendo la fecha actual"
date:                  2024-01-20T15:16:26.420829-07:00
html_title:           "Bash: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?

Obtener la fecha actual en Rust significa acceder al momento presente en el sistema. Los programadores lo hacen para registros, medidas de tiempo, o funciones que dependen de la fecha.

## Cómo:

```Rust
use chrono::{Local, Datelike};

fn main() {
    let hoy = Local::today();
    println!("Hoy es: {}", hoy.format("%Y-%m-%d"));  // Formato: Año-Mes-Día
}

// Salida esperada (varía según el día en que ejecutes el código):
// Hoy es: 2023-04-07
```

## Análisis Profundo

Históricamente, Rust siempre ha valorado la seguridad y la precisión en el manejo del tiempo, y la biblioteca `chrono` es el estándar de facto para trabajar con fechas y horas. Aunque la biblioteca estándar de Rust incluye funcionalidades básicas para manejar el tiempo a través del módulo `std::time`, `chrono` ofrece abstracciones más ricas y fáciles de usar.

Alternativas incluyen el uso de la biblioteca `time`, que también tiene funciones para manejar tiempo y fechas, pero puede tener diferencias en la API y en cómo se manejan las zonas horarias.

En cuanto a implementación, `chrono` maneja fechas con tipos robustos como `DateTime` para fechas y horas, y `Date` para solo fechas, cada uno con asociaciones a zonas horarias específicas o al horario universal coordinado (UTC). Es importante tener en cuenta la zona horaria; `Local::today()` devuelve la fecha actual en la zona horaria local de la máquina donde se ejecuta el código.

## Ver También

- Documentación oficial de `chrono`: https://docs.rs/chrono/
- Crates.io, donde puedes encontrar la biblioteca `chrono`: https://crates.io/crates/chrono
- Módulo oficial de Rust para el tiempo: https://doc.rust-lang.org/std/time/index.html
- Documentación alternativa para la biblioteca `time`: https://docs.rs/time/
