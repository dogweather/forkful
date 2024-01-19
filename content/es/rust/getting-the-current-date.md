---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Programar En Rust: Cómo obtener la fecha actual

## ¿Qué y Por Qué?
Obtener la fecha actual en la programación es el proceso de acceder a la fecha y hora del sistema actual. Los programadores suelen hacerlo para registrar eventos, marcar el tiempo o realizar cálculos basados en fechas.

## Cómo hacerlo:
Primero, usa el módulo `chrono` para manipular fechas y horas. Para instalarlo añade `chrono = "0.4"` a tu archivo `Cargo.toml`.

A continuación te mostramos cómo obtener la fecha actual en Rust:

```Rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let now = Utc::now();
    println!("{}", now);
}
```
Este código imprimirá la fecha y hora UTC actual, como:

```
2022-02-15T08:45:23.323422901Z
```

## Profundización:
Durante mucho tiempo, los programadores de Rust han utilizado `time` para obtener la fecha y hora. Sin embargo, `chrono` es ahora la opción preferida debido a su mayor funcionalidad y facilidad de uso. Mientras que `time` solo proporciona tiempo en segundos desde la época UNIX, `chrono` ofrece una gran cantidad de funcionalidades adicionales, como la capacidad de representar fechas, horas e instantes, así como periodos y duraciones.

Una alternativa a `chrono` es `time`, otro módulo de Rust. Aunque es menos versátil, puede ser suficiente para proyectos más simples.

A nivel de implementación, cuando solicitas la fecha y hora actual, `chrono` recurre a llamadas al sistema de bajo nivel que interactúan con el reloj de tu sistema operativo.

## Ver También:
Si quieres profundizar más, te recomiendo los siguientes recursos:

1. Documentación oficial de Chrono: https://docs.rs/chrono/0.4.19/chrono/
2. Tutorial de Rust sobre el manejo de fecha y hora: https://www.tutorialspoint.com/rust/rust_date_time.htm
3. Documentación de la librería Time: https://docs.rs/time/0.2.16/time/