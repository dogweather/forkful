---
title:                "Comparando dos fechas"
html_title:           "Rust: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas puede ser una tarea común en programas y aplicaciones, ya sea para calcular la diferencia entre ellas, verificar si una fecha está antes o después de otra, o simplemente para mostrarlas de manera ordenada en una interfaz. En este artículo, aprenderemos cómo podemos comparar dos fechas en Rust de manera sencilla y eficiente.

## Cómo

Para comparar dos fechas en Rust, podemos utilizar el tipo de dato `chrono::DateTime`, que nos permite representar una fecha y hora específica. Primero, debemos importar la librería `chrono` en nuestro proyecto:

```Rust
use chrono::{DateTime, Utc};
```

A continuación, creamos dos instancias de `DateTime`, una para cada fecha que queremos comparar:

```Rust
let fecha_1 = DateTime::parse_from_rfc3339("2021-01-01T12:00:00+00:00").unwrap();
let fecha_2 = DateTime::parse_from_rfc3339("2021-03-17T00:00:00+00:00").unwrap();
```

Luego, podemos utilizar operadores de comparación, como `>`, `<`, `>=` y `<=`, para determinar si una fecha es mayor, menor, mayor o igual, o menor o igual que la otra:

```Rust
if fecha_1 > fecha_2 {
    // fecha_1 es posterior a fecha_2
} else if fecha_1 < fecha_2 {
    // fecha_1 es anterior a fecha_2
} else if fecha_1 >= fecha_2 {
    // fecha_1 es posterior o igual a fecha_2
} else {
    // fecha_1 es anterior o igual a fecha_2
}
```

También podemos utilizar el método `cmp` para obtener un resultado `std::cmp::Ordering`, que nos indica si una fecha es mayor, menor o igual a la otra:

```Rust
match fecha_1.cmp(&fecha_2) {
    std::cmp::Ordering::Greater => println!("fecha_1 es posterior a fecha_2"),
    std::cmp::Ordering::Less => println!("fecha_1 es anterior a fecha_2"),
    std::cmp::Ordering::Equal => println!("fecha_1 y fecha_2 son iguales"),
}
```

## Deep Dive

La librería `chrono` también nos ofrece métodos para realizar operaciones más específicas en nuestras fechas, como calcular la diferencia entre ellas y convertirlas a otros formatos. Por ejemplo, si queremos saber cuántos días han pasado entre dos fechas, podemos utilizar el método `signed_duration_since` y luego obtener la cantidad de días mediante el método `num_days`:

```Rust
let diferencia = fecha_2.signed_duration_since(fecha_1);
let dias = diferencia.num_days();
println!("Han pasado {} días entre fecha_1 y fecha_2", dias);
```

También podemos convertir nuestras fechas a otros formatos, como `Local`, que nos permite obtener la fecha y hora en una zona horaria específica:

```Rust
let fecha_local = fecha_1.with_timezone(&chrono::Local);
println!("La fecha y hora en mi zona horaria es: {}", fecha_local);
```

Para más información sobre cómo trabajar con fechas y la librería `chrono`, puedes consultar la documentación oficial: https://docs.rs/chrono/.

## Ver también

- https://www.red-bean.com/identified/2.4k/date_comparison_lol.html
- https://doc.rust-lang.org/std/ops/enum.Ordering.html