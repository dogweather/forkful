---
title:                "Rust: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado puede ser útil en muchas situaciones, como programación de citas, gestión de proyectos o planificación de eventos. En este artículo, aprenderemos a calcular fechas en Rust utilizando la librería estándar.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, necesitamos tener una fecha inicial y una cantidad de tiempo (en segundos, minutos, horas, días, etc.) que queremos sumar o restar. En Rust, podemos hacer esto utilizando el tipo de datos `Duration` de la librería estándar, que representa una cantidad de tiempo.

Primero, importemos la librería `chrono` y sus tipos de datos `DateTime` y `Duration`:

```Rust
use chrono::{DateTime, Duration, Utc};
```

Luego, creamos una fecha inicial utilizando `DateTime::parse_from_str()` y especificando un formato de fecha:

```Rust
let fecha_inicial = DateTime::parse_from_str("2021/08/25 15:30:00", "%Y/%m/%d %H:%M:%S").unwrap();
```

Ahora, podemos utilizar la función `checked_add()` o `checked_sub()` de `Duration` para sumar o restar una cantidad de tiempo a nuestra fecha inicial. Por ejemplo, para obtener la fecha 2 horas en el futuro, podemos hacer lo siguiente:

```Rust
let fecha_en_el_futuro = fecha_inicial.checked_add(Duration::hours(2)).unwrap();
```

O para obtener la fecha 3 días en el pasado:

```Rust
let fecha_en_el_pasado = fecha_inicial.checked_sub(Duration::days(3)).unwrap();
```

También podemos hacer cálculos más complejos combinando diferentes unidades de tiempo, por ejemplo:

```Rust
let fecha_futura_poblacion = fecha_inicial.checked_add(Duration::days(365 * 50) // 50 años
                                                + Duration::hours(1500) // 1500 horas
                                                + Duration::minutes(10000)); // 10000 minutos
```

## Inmersión profunda

La librería `chrono` nos ofrece una amplia gama de funciones para manejar fechas y tiempos de manera sencilla y precisa. Además de `checked_add()` y `checked_sub()`, también podemos utilizar funciones como `add()` y `sub()` que devuelven una nueva fecha en lugar de utilizar un tipo `DateTime`. Además, podemos especificar el formato de fecha y hora que queremos utilizar en las funciones `parse_from_str()` y `format()`.

También podemos hacer cálculos con fechas en diferentes zonas horarias utilizando tipos como `FixedOffset` y `Local` en lugar de `Utc`. Y si necesitamos trabajar con fechas más precisas, la librería `chrono` también proporciona tipos de datos como `NaiveDate` y `Utc::now()` para manejar microsegundos y nanosegundos.

## Ver también

- Documentación oficial de `chrono`: https://docs.rs/chrono/latest/chrono/
- Tutorial de Rust sobre fechas y tiempos: https://www.youtube.com/watch?v=IkdYT