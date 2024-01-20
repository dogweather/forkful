---
title:                "Analizando una fecha desde una cadena de texto"
html_title:           "PHP: Analizando una fecha desde una cadena de texto"
simple_title:         "Analizando una fecha desde una cadena de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Análisis de fechas desde una cadena en Rust

## ¿Qué y por qué?

En programación, analizar una fecha a partir de una cadena implica convertir datos textuales en tipo `Datetime` que comprende Rust bien. Este proceso es útil para manipular y almacenar fechas ingresadas por el usuario de forma más eficiente.

## ¿Cómo se hace?

A continuación se muestran unos ejemplos en Rust para analizar fechas a partir de cadenas.

```Rust
extern crate chrono;
use chrono::*;

fn main() {
    let fecha_texto = "2022-02-01";
    let fecha_objeto = NaiveDate::parse_from_str(fecha_texto, "%Y-%m-%d").unwrap();
    println!("{:?}", fecha_objeto);
}
```
Cuando se ejecuta este código, saldrá:

```Rust
NaiveDate { ymd: YMD { year: 2022, month: 2, day: 1 } }
```

## Profundización

Desde los primeros días de la informática, el análisis de fechas ha sido fundamental para manejar las fechas de entrada generadas por los usuarios. Rust, al igual que otros lenguajes de programación, ofrece un manejo robusto de fechas, utilizando bibliotecas como `chrono`.

Existen múltiples alternativas para analizar las fechas. Algunos utilizan bibliotecas de terceros, mientras que otros prefieren hacer sus propias implementaciones.

Para entender las implementaciones internas, Rust utiliza una secuencia de coincidencias para comparar y transformar las partes de la cadena en una fecha. Los errores de coincidencia como una entrada inválida o un formato de fecha incorrecto se manejan utilizando `Result`, que proporciona un retorno de `Ok` para una fecha válida o un `Error` para una entrada inválida.

## Ver también

- Documentación oficial de chrono: https://docs.rs/chrono/0.4.19/chrono/
- Tutorial de Rust sobre manejo de fechas y tiempos: https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html
- Discusión de StackOverflow sobre análisis de fechas en Rust: https://stackoverflow.com/questions/27424139/how-do-i-parse-a-string-to-a-date