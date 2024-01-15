---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Rust: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado cómo sería tu vida si pudieras viajar en el tiempo? Aunque aún no es posible físicamente, en la programación podemos hacer algo similar. Podemos calcular y obtener fechas en el futuro o en el pasado utilizando lenguajes de programación como Rust. Ya sea para fines prácticos o simplemente por diversión, saber cómo calcular fechas puede ser una habilidad útil para tener en tu kit de herramientas de programación.

## Cómo Hacerlo
```rust
use chrono::{DateTime, Duration, Utc};

fn calcular_fecha(ano: i32, mes: u32, dia: u32, dias_a_sumar: i64) -> DateTime<Utc> {
    let fecha = Utc.ymd(ano, mes, dia);
    fecha + Duration::days(dias_a_sumar)
}

fn main() {
    let fecha_actual = Utc::now();
    // Calcular fecha 30 días en el futuro
    let fecha_futura = calcular_fecha(fecha_actual.year(), fecha_actual.month(), fecha_actual.day(), 30);
    // Calcular fecha 14 días en el pasado
    let fecha_pasada = calcular_fecha(fecha_actual.year(), fecha_actual.month(), fecha_actual.day(), -14);
    
    // Imprimir fechas
    println!("Fecha Actual: {}", fecha_actual);
    println!("Fecha Futura: {}", fecha_futura);
    println!("Fecha Pasada: {}", fecha_pasada);
}
```

### Resultado:
```
Fecha Actual: 2021-05-29 21:18:47.895860+00:00
Fecha Futura: 2021-06-28 21:18:47.895860+00:00
Fecha Pasada: 2021-05-15 21:18:47.895860+00:00
```

## Inmersión Profunda
En Rust, podemos utilizar la librería `chrono` para manejar fechas y horas. Esta librería nos permite crear una fecha utilizando el método `Utc.ymd()`, el cual recibe como argumentos el año, mes y día en ese orden. Además, podemos utilizar el método `Duration::days()` para sumar o restar días a una fecha determinada.

Es importante tener en cuenta que el resultado de la función `calcular_fecha()` es un objeto de tipo `DateTime<Utc>`. Esto significa que la fecha está ajustada a la zona horaria UTC (Coordinated Universal Time) en lugar de utilizar la zona horaria local del sistema.

## Ver También
- [Documentación oficial de la librería `chrono`](https://docs.rs/chrono)
- [Tutorial sobre el manejo de fechas en Rust](https://doc.rust-lang.org/std/time)
- [Guía para principiantes de Rust](https://www.rust-lang.org/learn/get-started)