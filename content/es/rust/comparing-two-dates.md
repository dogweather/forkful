---
title:                "Rust: Comparando dos fechas"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Por qué comparar dos fechas en Rust?

Comparar fechas puede ser una tarea común en muchos proyectos de programación, especialmente en aplicaciones que manejan datos sensibles y requieren un seguimiento de cuándo se crearon o modificaron ciertos registros. En Rust, el proceso de comparar fechas puede ser realizado de manera eficiente y segura gracias a sus características únicas de lenguaje.

## Cómo comparar dos fechas en Rust

Para comparar dos fechas en Rust, primero debemos convertirlas a objetos de tipo `DateTime`. Esto se puede lograr con la ayuda de la librería `chrono`, que proporciona funciones para manejar fechas y horas en Rust. A continuación, se muestra un ejemplo de cómo comparar dos fechas en Rust:

```
use chrono::{DateTime, Utc, Duration};

let fecha_uno = DateTime::parse_from_str("2021-09-01 10:00:00", "%Y-%m-%d %H:%M:%S")
    .unwrap()
    .with_timezone(&Utc);
let fecha_dos = DateTime::parse_from_str("2021-09-02 12:00:00", "%Y-%m-%d %H:%M:%S")
    .unwrap()
    .with_timezone(&Utc);

if fecha_uno < fecha_dos {
    println!("La fecha uno es anterior a la fecha dos.");
} else if fecha_uno > fecha_dos {
    println!("La fecha uno es posterior a la fecha dos.");
} else {
    println!("Ambas fechas son iguales.");
}
```

La salida de este código sería: "La fecha uno es anterior a la fecha dos."

## Profundizando en la comparación de fechas en Rust

Una vez que entendemos cómo comparar fechas en Rust, podemos profundizar en algunas de sus características más únicas. Por ejemplo, la librería `chrono` también proporciona métodos para calcular la diferencia en tiempo entre dos fechas, incluso tomando en cuenta la zona horaria. Además, en Rust también es posible utilizar la función `Duration::since()` para obtener la cantidad de tiempo transcurrido desde una fecha específica hasta el momento actual.

## Ver también

Para obtener más información sobre la comparación de fechas en Rust, puedes consultar los siguientes recursos:

- Documentación de la librería `chrono`: https://docs.rs/chrono/latest/chrono/
- Ejemplos de fechas y horas en Rust: https://rosettacode.org/wiki/Date_and_time#Rust
- Artículo sobre cómo manejar fechas y horas en Rust: https://blog.logrocket.com/handling-dates-and-times-in-rust/