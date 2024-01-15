---
title:                "Obteniendo la fecha actual"
html_title:           "Rust: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

¿Has necesitado alguna vez obtener la fecha y hora actual en tus programas de Rust? Obtener la fecha actual es una tarea común en la programación y en este artículo te mostraremos cómo hacerlo de manera sencilla y eficiente utilizando Rust.

## Cómo hacerlo

En Rust, podemos obtener la fecha y hora actual utilizando el módulo `std::time`. Primero, importamos este módulo en nuestro programa:

```Rust
use std::time;
```

Luego, podemos usar la función `now()` para obtener un objeto `SystemTime` que representa la fecha y hora actuales. Podemos almacenar este objeto en una variable y luego utilizar sus métodos para obtener información específica como la fecha, la hora o incluso el día de la semana.

```Rust
let fecha_hora_actual = time::SystemTime::now();
```

Para obtener la fecha actual, podemos usar el método `date()`, el cual nos devuelve un objeto `Date` que contiene el año, mes y día.

```Rust
let fecha_actual = fecha_hora_actual.date();
println!("La fecha actual es: {}", fecha_actual);
```

Si queremos obtener la hora actual, podemos usar el método `time()`, que nos devuelve un objeto `Duration` que contiene la cantidad de segundos y nanosegundos transcurridos desde la medianoche.

```Rust
let hora_actual = fecha_hora_actual.time();
println!("La hora actual es: {}", hora_actual);
```

Incluso podemos obtener el día de la semana actual utilizando el método `weekday()`, el cual nos devuelve un enum `Weekday` que representa el día de la semana (domingo, lunes, martes, etc.).

```Rust
let dia_actual = fecha_hora_actual.weekday();
println!("Hoy es: {}", dia_actual);
```

## Profundizando

El módulo `std::time` también nos permite realizar operaciones con las fechas y horas, como calcular la diferencia entre dos fechas o sumar una cierta cantidad de tiempo a una fecha dada.

Por ejemplo, si queremos calcular la cantidad de segundos que han pasado entre dos fechas, podemos utilizar el método `elapsed()`:

```Rust
let fecha_pasada = time::SystemTime::now() - time::Duration::from_secs(1000);
let fecha_actual = time::SystemTime::now();

let segundos_pasados = fecha_actual.elapsed().unwrap().as_secs();
println!("Han pasado {} segundos desde la fecha pasada", segundos_pasados);
```

También podemos sumar una cantidad de tiempo determinada a una fecha específica. Por ejemplo, si queremos obtener la fecha y hora en 10 días, podemos utilizar el método `checked_add()`:

```Rust
let fecha_actual = time::SystemTime::now();
let fecha_futura = fecha_actual.checked_add(time::Duration::from_secs(864000));
println!("La fecha y hora en 10 días será: {:?}", fecha_futura);
```

## Ver también

- [Documentación oficial de Rust sobre el módulo `std::time`](https://doc.rust-lang.org/std/time/index.html)
- [Tutorial sobre cómo trabajar con fechas y horas en Rust](https://www.wezm.net/technical/2019/10/use-rust-serde-json-value/)
- [Ejemplos prácticos de cómo utilizar el módulo `std::time`](https://dev.to/twiecki/how-to-get-dates-and-times-right-in-rust-5kma)