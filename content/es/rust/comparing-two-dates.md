---
title:                "Rust: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas es una tarea común en la programación y puede resultar especialmente útil en aplicaciones que manejan eventos o tareas programadas. En este blog post, aprenderemos cómo hacerlo en Rust de una manera eficiente y fácil de entender.

## Cómo hacerlo

Para comparar dos fechas en Rust, podemos utilizar el tipo de datos `NaiveDateTime` de la biblioteca `chrono`. Primero, importamos la biblioteca en nuestro código:

```
use chrono::NaiveDateTime;
```

Luego, necesitamos crear nuestras dos fechas a comparar. Podemos hacerlo especificando el año, mes, día, hora, minutos y segundos de cada fecha, o bien usando una cadena de texto con el formato adecuado:

```
let fecha_1 = NaiveDateTime::parse_from_str("2021-01-01 12:00:00", "%Y-%m-%d %H:%M:%S").unwrap();
let fecha_2 = NaiveDateTime::new(2021, 1, 1, 12, 30, 0);
```

Una vez que tenemos nuestras fechas, podemos compararlas utilizando los operadores de comparación `==`, `!=`, `<`, `>`, `<=` y `>=`, los cuales están implementados para el tipo `NaiveDateTime`. Por ejemplo:

```
if fecha_1 < fecha_2 {
    println!("La fecha 1 es anterior a la fecha 2");
}
```

## Profundizando

Además de los operadores de comparación, la biblioteca `chrono` ofrece otras funciones útiles para trabajar con fechas y horas. Por ejemplo, podemos obtener la diferencia entre dos fechas en segundos, minutos, horas, días, etc. También podemos realizar operaciones aritméticas como sumar o restar una cierta cantidad de tiempo a una fecha. Para más información, se recomienda consultar la documentación oficial de la biblioteca.

## Ver también

- [Documentación oficial de la biblioteca `chrono`](https://docs.rs/chrono/)
- [Tutorial de Rust: Trabajando con fechas y horas](https://www.rust-lang.org/es-ES/learn/tri