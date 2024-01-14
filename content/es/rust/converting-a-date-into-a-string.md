---
title:                "Rust: Convirtiendo una fecha en una cadena."
simple_title:         "Convirtiendo una fecha en una cadena."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

La conversión de una fecha en una cadena de texto es una tarea común en el desarrollo de aplicaciones. Puede ser necesario mostrar una fecha en un formato específico o almacenarla en una base de datos. En este artículo, exploraremos cómo realizar esta conversión utilizando Rust.

## Cómo hacerlo

Para convertir una fecha en una cadena de texto en Rust, primero necesitamos importar el módulo `chrono` que proporciona herramientas para trabajar con fechas y horas.

```Rust
use chrono::{DateTime, Local, TimeZone, NaiveDate, format::strftime};
```

A continuación, necesitamos crear una instancia de `DateTime` con la fecha que queremos convertir. Esto se puede hacer utilizando el método `now()` para obtener la fecha y hora actual o creando una nueva instancia con `DateTime::new()` pasando los valores de año, mes, día, hora, minuto y segundo.

```Rust
let today = Local::now();
let date = DateTime::new(2021, 10, 5, 8, 30, 0);
```

Una vez que tenemos nuestra fecha, podemos utilizar el método `format()` para especificar el formato de salida. Por ejemplo, si queremos obtener la fecha en formato `d/m/Y` (día/mes/año), podemos hacer lo siguiente:

```Rust
let formatted_date = date.format("%d/%m/%Y").to_string();
println!("{}", formatted_date); // Output: 05/10/2021
```

También podemos obtener la fecha en formato ISO utilizando el método `to_rfc3339()`.

```Rust
let iso_date = date.to_rfc3339();
println!("{}", iso_date); // Output: 2021-10-05T08:30:00+00:00
```

Sin embargo, si queremos un formato personalizado, podemos utilizar el método `strftime()` y pasar el formato deseado. Por ejemplo, si queremos obtener la fecha en formato `D de MMMM de YYYY` (día de mes de año), podemos hacer lo siguiente:

```Rust
let custom_date = date.strftime("%e de %B de %Y").unwrap();
println!("{}", custom_date); // Output: 5 de octubre de 2021
```

## Profundizando

La biblioteca `chrono` en realidad utiliza el tipo de datos `NaiveDateTime`, que representa una fecha y hora sin información de zona horaria. Esto permite trabajar con fechas y horas en diferentes formatos y zonas horarias.

Para obtener más información sobre cómo utilizar `chrono` y sus diferentes métodos de formato de fecha, se recomienda consultar su documentación oficial.

## Ver también

- [Documentación de Chrono](https://docs.rs/chrono/latest/chrono/)
- [Conversión de fechas en Rust](https://docs.rs/chrono/0.4.19/chrono/duration/struct.Duration.html) (en inglés)
- [Manipulación de fechas en Rust: un tutorial práctico](https://dzone.com/articles/date-and-time-manipulation-in-rust-a-practical-gu) (en inglés)