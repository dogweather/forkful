---
title:                "Analizando una fecha de una cadena"
html_title:           "Rust: Analizando una fecha de una cadena"
simple_title:         "Analizando una fecha de una cadena"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Parsing o "analizar" una fecha de una cadena de texto es el proceso de convertir una cadena que representa una fecha en un formato utilizado por una computadora para realizar cálculos y manipulaciones. Los programadores a menudo lo hacen para convertir fechas ingresadas por usuarios en un formato legible para la computadora, o para extraer componentes específicos de una fecha, como el mes o el año.

## ¡Así se hace!
```Rust
use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, ParseResult, Timelike, Utc, Weekday};

// parsing a date from string using DateTime
// example string: "2020-08-31T11:30:00Z"
let date_time: ParseResult<DateTime<Utc>> = "2020-08-31T11:30:00Z".parse();
match date_time {
    Ok(dt) => println!("Parsed time is {:?}", dt),
    Err(e) => println!("{:?}", e),
}

// parsing a date from string using NaiveDateTime
// example string: "2020-08-31 11:30:00"
let naive_date_time: ParseResult<NaiveDateTime> = NaiveDateTime::parse_from_str("2020-08-31 11:30:00", "%Y-%m-%d %H:%M:%S");
match naive_date_time {
    Ok(ndt) => println!("Parsed time is {:?}", ndt),
    Err(e) => println!("{:?}", e),
}

// parsing a date from string using NaiveDate
// example string: "2020-08-31"
let naive_date: ParseResult<NaiveDate> = "2020-08-31".parse();
match naive_date {
    Ok(nd) => println!("Parsed date is {:?}", nd),
    Err(e) => println!("{:?}", e),
}

// parsing a time from string using NaiveTime
// example string: "11:30:00"
let naive_time: ParseResult<NaiveTime> = NaiveTime::parse_from_str("11:30:00", "%H:%M:%S");
match naive_time {
    Ok(nt) => println!("Parsed time is {:?}", nt),
    Err(e) => println!("{:?}", e),
}

// extracting components from a parsed time
// date: 2020-08-31T11:30:00Z
let parsed_time: DateTime<Utc> = date_time.unwrap();
println!("Year: {}", parsed_time.year());
println!("Month: {}", parsed_time.month());
println!("Day: {}", parsed_time.day());
println!("Hour: {}", parsed_time.hour());
println!("Minute: {}", parsed_time.minute());
println!("Second: {}", parsed_time.second());
println!("Timezone: {:?}", parsed_time.timezone());
println!("Day of week: {:?}", parsed_time.weekday());

```

## Todo lo que debes saber
Para entender a fondo la importancia de parsing de fechas en la programación, es útil conocer su origen histórico. En el pasado, las computadoras tenían dificultades para manejar fechas debido a sus diferentes formatos y regiones, lo que dificultaba la realización de cálculos y manipulaciones precisas. Sin embargo, con el desarrollo de estándares internacionales y bibliotecas como Chrono en Rust, el parsing de fechas es ahora más fácil y preciso.

Otra alternativa para parsing de fechas en Rust es la biblioteca "time", que ofrece una función específica para analizar fechas a partir de una cadena de texto.

En cuanto a la implementación, Chrono utiliza la biblioteca "datetime" para realizar el análisis, que utiliza un enfoque basado en expresiones regulares para reconocer patrones de fecha y convertirlos en los componentes correspondientes.

## ¡Echa un vistazo!
- Documentación de Chrono: https://docs.rs/chrono/latest/chrono/
- Documentación de Time: https://docs.rs/time/latest/time/
- Ejemplo de parsing de fechas en Rust: https://gist.github.com/wangjiaxi/abd7d9e0859a29a881368baec7fb382f.