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

## Qué y por qué?
Calcular una fecha en el futuro o pasado es una tarea común para los programadores. Esta acción implica determinar una fecha basada en una fecha de origen y un número específico de días, meses o años del futuro o pasado. Los programadores realizan esta tarea para diversas aplicaciones, como por ejemplo, programación de citas, cálculo de plazos de entrega o creación de calendarios personalizados.

## Cómo:
Para calcular una fecha en Rust, se puede usar la librería `chrono`. Primero, se debe importar la librería en el código:
```Rust
use chrono::{NaiveDate, Duration};
```
Luego, se puede crear una fecha de origen y usar la función `checked_add_signed` para calcular una fecha en el futuro o `checked_sub_signed` para una fecha en el pasado:
```Rust
let date = NaiveDate::from_ymd(2020, 03, 15); //fecha de origen
let new_date = date.checked_add_signed(Duration::days(30)); //fecha en el futuro (30 días después)
let new_date_past = date.checked_sub_signed(Duration::weeks(2)); //fecha en el pasado (2 semanas antes)
```
La salida de este código mostrará la fecha correspondiente en formato `yyyy-mm-dd`.

## Profundizando:
La necesidad de calcular fechas en el futuro o pasado surge de la necesidad de automatizar tareas relacionadas con el tiempo y la planificación. Antes de la llegada de las librerías como `chrono`, los programadores tenían que realizar cálculos manuales para obtener estas fechas, lo que llevaba más tiempo y aumentaba la posibilidad de cometer errores.

Otra forma de calcular fechas en Rust es usando la librería `time`. Sin embargo, `chrono` es considerada como una alternativa más moderna y recomendada para estos cálculos.

En cuanto a la implementación, `chrono` utiliza el progreso de tiempo actual de Unix como base para todas las funciones de cálculo de tiempo. Esta medida de tiempo se basa en el número de segundos transcurridos desde el 1 de enero de 1970. Al utilizar incrementos o decrementos de tiempo en segundos, la librería puede calcular fechas en el futuro o pasado con precisión.

## Ver también:
- Documentación oficial de `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- Cálculo de fechas en Rust con `time`: https://crates.io/crates/time