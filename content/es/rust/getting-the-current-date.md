---
title:                "Rust: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

En el mundo de la programación, conocer la fecha actual es una tarea muy común. Ya sea para llevar el registro de eventos, mostrar información personalizada o simplemente para saber en qué día vivimos, la obtención de la fecha actual es un aspecto fundamental en muchos proyectos. Por eso, en este post hablaremos de cómo obtener la fecha actual utilizando Rust.

## Cómo hacerlo

Para obtener la fecha actual en Rust, utilizaremos el módulo `chrono`. Este módulo proporciona una estructura de datos llamada `DateTime` que nos permite almacenar y manipular fechas y horas. Primero, agreguemos la dependencia de `chrono` en nuestro archivo `Cargo.toml`:

```
[dependencies]
chrono = "0.4.19"
```

A continuación, importamos el módulo en nuestro código:

```
use chrono::{DateTime, Local};
```

Para obtener la fecha actual, simplemente creamos una instancia de `DateTime` y la asignamos a la hora local actual:

```
let fecha_actual: DateTime<Local> = Local::now();
```

Podemos imprimir la fecha actual utilizando el método `format()` y proporcionándole un formato de fecha como parámetro:

```
println!("La fecha actual es: {}", fecha_actual.format("%d/%m/%Y"));
```

Este es solo uno de los muchos formatos que se pueden utilizar para imprimir fechas. Puedes explorar más opciones en la documentación de `chrono`.

## Profundizando

Si queremos obtener más información sobre la fecha actual, podemos acceder a sus componentes individuales utilizando los métodos `day()`, `month()`, `year()`, `hour()`, `minute()` y `second()`. Por ejemplo, si queremos imprimir el día y mes actual, podemos hacerlo de la siguiente manera:

```
println!("Hoy es {} de {}", fecha_actual.day(), fecha_actual.month());
```

También podemos manipular la fecha actual utilizando los métodos `with_day()`, `with_month()` y `with_year()`, que nos permiten cambiar el día, mes y año de la fecha.

## Ver también

- [Documentación de Chrono](https://docs.rs/chrono/0.4.19/chrono/)
- [Tutorial de Rust: Date and Time](https://www.tutorialspoint.com/rust/rust_date_time.htm)
- [Obtener la fecha y hora actual en Rust](https://www.techiedelight.com/get-current-date-time-rust/)

¡Ahora puedes obtener la fecha actual en tus próximos proyectos con Rust! Esperamos que este post te haya sido útil y te invitamos a seguir aprendiendo más sobre este lenguaje de programación en nuestros próximos artículos. ¡Hasta la próxima!