---
title:                "Rust: Cálculo de una fecha en el futuro o pasado"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o pasado es una habilidad importante para cualquier programador, ya que se puede aplicar a una variedad de situaciones, como planificación de proyectos o análisis de datos.

## Cómo hacerlo

La forma más sencilla de calcular una fecha en el futuro o pasado es utilizando la biblioteca estándar de Rust [chrono](https://crates.io/crates/chrono). Primero, es necesario agregarla a tu proyecto en el archivo `Cargo.toml`:

```
[dependencias]
chrono = "0.4.13"
```

A continuación, importa la biblioteca en tu archivo de código y crea una nueva instancia de `DateTime` con la fecha y hora actuales:

```Rust
use chrono::{Local, Datelike, Timelike};

let now = Local::now();
```

Para calcular una fecha en el futuro, puedes utilizar la función `with_future` y especificar la cantidad de días que deseas sumar a la fecha actual:

```Rust
let fecha_futura = now.with_future(3);
```

Para calcular una fecha en el pasado, utiliza la función `with_past` y especifica la cantidad de días que deseas restar a la fecha actual:

```Rust
let fecha_pasada = now.with_past(7);
```

También es posible realizar cálculos más complejos, como sumar o restar años, meses o incluso horas a una fecha:

```Rust
let fecha_futura = now.with_year(2022).unwrap()
                    .with_month(12).unwrap()
                    .with_hour(8).unwrap();
```

Finalmente, puedes imprimir la fecha resultante utilizando la función `format` y especificando el formato deseado:

```Rust
println!("{}", fecha_futura.format("%A, %B %d, %Y"));
// Output: Friday, December 23, 2022
```

## Profundizando

La biblioteca `chrono` ofrece una amplia gama de funciones y métodos para realizar cálculos de fechas en Rust. Puedes explorar su documentación completa [aquí](https://docs.rs/chrono/0.4.13/chrono/).

Otra opción es utilizar la biblioteca [date_time](https://crates.io/crates/date_time), que ofrece una sintaxis similar a la de `chrono`, pero con algunas características adicionales.

## Ver también

- [Documentación de chrono en crates.io](https://docs.rs/chrono/0.4.13/chrono/)
- [Documentación de date_time en crates.io](https://docs.rs/date_time/0.2.25/date_time/)
- [Guía de Rust: Trabajar con fechas y horas](https://www.rust-lang.org/es-ES/learn/training/trabajando-con-fechas-y-horas)