---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Rust: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Por qué convertir una fecha en una cadena de texto?

Si estás trabajando en un proyecto que requiere el manejo de fechas, es probable que en algún punto necesites convertir una fecha en una cadena de texto. Esto puede ser útil para mostrar la fecha en un formato específico o para realizar cálculos de tiempo. En este artículo, aprenderás cómo hacerlo en Rust de forma rápida y sencilla.

## Cómo hacerlo

La forma más sencilla de convertir una fecha en una cadena de texto en Rust es utilizando la función `format!()`. Esta función utiliza una sintaxis similar a la de `println!()` y te permite especificar el formato en el que quieres mostrar la fecha.

Dentro de `format!()`, utilizamos placeholders `{:...}` para indicar qué partes de la fecha queremos mostrar y cómo queremos mostrarlas. Por ejemplo, para mostrar la fecha en formato "día/mes/año", podemos usar `{:d}/{:m}/{:y}`. 

Veamos un ejemplo completo de cómo convertir una fecha en una cadena de texto en Rust:

```Rust
use chrono::prelude::*;

fn main() {
    // Creamos una fecha: 3 de mayo de 2021 a las 12:00
    let date = Utc.ymd(2021, 5, 3).and_hms(12, 0, 0);

    // Convertimos la fecha en una cadena de texto con el formato deseado
    let date_string = format!("{:m}/{:d}/{:y} a las {:H}:{:M}", date);

    // Mostramos la cadena de texto resultante
    println!("{}", date_string);
}
```

El resultado de este código sería `5/3/21 a las 12:00`.

## Profundizando

Ahora que ya sabes cómo convertir una fecha en una cadena de texto en Rust, es importante entender que esta función se basa en el módulo `chrono`, que es una librería de manejo de fechas y tiempos en Rust. Esta librería ofrece muchas más opciones y formatos para trabajar con fechas, como por ejemplo, el manejo de diferentes zonas horarias.

Al utilizar `chrono`, también tienes la posibilidad de mostrar la fecha en otros idiomas, siempre y cuando se hayan instalado los correspondientes paquetes de idiomas en tu sistema.

Puedes consultar la documentación del módulo `chrono` para obtener más información sobre todas las opciones de formato y funcionalidades que ofrece.

## Ver también

- Documentación de `chrono`: https://docs.rs/chrono/
- Tutorial de manejo de fechas en Rust: https://blog.knoldus.com/handling-datetime-in-rust-with-chrono/
- Repositorio de GitHub de `chrono`: https://github.com/chronotope/chrono