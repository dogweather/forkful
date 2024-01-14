---
title:                "Rust: Convirtiendo una fecha en una cadena"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

La conversión de una fecha en una cadena de texto es una tarea común en la programación. Puede ser necesario para su uso en informes, interfaces de usuario, o simplemente para una mejor visualización de la información en la terminal. En Rust, podemos hacer esta conversión de manera eficiente y precisa utilizando sus poderosas funciones integradas.

## Cómo hacerlo

En Rust, podemos convertir una fecha en una cadena de texto utilizando el método `format` de la estructura `DateTime`. Aquí hay un ejemplo básico:

```Rust
use chrono::prelude::*;

fn main() {
    let date = Utc::now();
    let formatted_date = date.format("%Y-%m-%d").to_string();
    println!("{}", formatted_date);
}
```
El código anterior utiliza la librería `chrono` para obtener la fecha y hora actual en UTC, y luego utiliza el método `format` para formatearla como una cadena de texto en el formato `AAAA-MM-DD`.

Podemos personalizar el formato de la cadena de texto al pasar diferentes argumentos al método `format`, como se muestra en el siguiente ejemplo:

```Rust
use chrono::prelude::*;

fn main() {
    let date = Utc::now();
    let formatted_date = date.format("%A, %d. %B %Y").to_string();
    println!("{}", formatted_date);
}
```

Este código producirá una cadena de texto que muestra el día de la semana, el día del mes, el nombre del mes y el año, como por ejemplo: "Monday, 02. August 2021".

## Profundizando

Para una mayor personalización, podemos utilizar la macro `format!` en lugar del método `format` para convertir una fecha en una cadena de texto. La macro `format!` nos permite incluir múltiples variables en una sola cadena de texto, como se muestra en el siguiente ejemplo:

```Rust
use chrono::prelude::*;

fn main() {
    let date = Utc::now();
    let month_num = date.format("%m").to_string();
    let month = match month_num.parse::<i32>() {
        Ok(num) => num,
        Err(_) => 1,
    };
    let formatted_date = format!("Hoy es {} de {_mes_actual}", date.day(), _mes_actual = month);
    println!("{}", formatted_date);
}
```

En este ejemplo, además de obtener el día de la fecha actual, también utilizamos la macro `format!` para obtener el número del mes y luego lo convertimos en un nombre de mes utilizando un "match".

## Ver También
- Documentación de Rust sobre fechas y horas: https://doc.rust-lang.org/std/time/
- Librería chrono para manipular fechas en Rust: https://github.com/chronotope/chrono
- Tutorial sobre formateo de fechas y horas en Rust: https://www.educative.io/blog/how-to-format-date-and-time-in-rust