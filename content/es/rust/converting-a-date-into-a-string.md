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

## ¿Qué y por qué?

Convertir una fecha en una cadena de texto es un proceso utilizado por los programadores para mostrar una fecha en un formato legible para los usuarios. Esto es particularmente útil en aplicaciones que requieren mostrar fechas en un formato específico.

## Cómo:

```Rust
use chrono::NaiveDate;
use chrono::format::strftime::StrftimeItems;

let date = NaiveDate::from_ymd(2021, 12, 25);
let format = StrftimeItems::new("%d de %B de %Y"); // formato en español
let string_date = date.format_with_items(format).to_string(); 
```

En el código anterior, utilizamos la biblioteca `chrono` para convertir una fecha en una cadena de texto en el formato especificado. Primero, creamos una fecha con el año, mes y día deseados. Luego, creamos un objeto `StrftimeItems` con el formato deseado, en este caso, `%d de %B de %Y` que muestra el día del mes, el nombre del mes y el año en español. Finalmente, utilizamos el método `format_with_items` para convertir la fecha en una cadena de texto y luego lo convertimos a un tipo de dato `String`.

## Inmersión profunda

La necesidad de convertir una fecha en una cadena de texto se origina en la forma en que las fechas se almacenan y procesan en la computadora. Las fechas se almacenan internamente como números, lo que facilita el procesamiento y cálculo de fechas. Sin embargo, para los usuarios, las fechas en formato numérico pueden ser difíciles de entender. Por lo tanto, los programadores utilizan la conversión a una cadena de texto con un formato legible para mejorar la experiencia del usuario.

Aparte de la biblioteca `chrono`, también hay otras formas de convertir fechas en cadenas de texto en Rust, como utilizando la librería `time` o escribir tu propia función de conversión. Sin embargo, `chrono` es una opción popular debido a su amplia funcionalidad y su soporte para distintos formatos y zonas horarias.

## Vea también

- Documentación oficial de `chrono`: https://docs.rs/chrono/latest/chrono/format/strftime/index.html
- Librería `time`: https://docs.rs/time/0.2.25/time/
- Ejemplo de función de conversión propia: https://stackoverflow.com/questions/52078568/converting-date-to-string-in-rust