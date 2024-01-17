---
title:                "Trabajando con archivos csv"
html_title:           "Rust: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## ¡Qué es y por qué!
Trabajar con archivos CSV (comma-separated values) se refiere a la manipulación de datos estructurados en forma de tabla, donde cada fila representa un registro y cada columna contiene un valor específico. Los programadores suelen trabajar con CSV como una forma de almacenar y procesar grandes cantidades de información, ya que este formato es fácil de entender y se puede utilizar en una variedad de aplicaciones.

## Cómo hacerlo:
````Rust
// Importamos la biblioteca para trabajar con CSV
use csv;

// Creamos un nuevo archivo CSV
csv::Writer::from_path("datos.csv")
    .unwrap();

// Añadimos una fila al archivo
records.push(vec!["Nombre", "Edad", "Ciudad"]);

// Guardamos el archivo
writer.write(records)
    .unwrap();
````

El resultado de este código sería un archivo CSV llamado "datos.csv" con una fila que contiene los datos "Nombre", "Edad" y "Ciudad".

## Profundizando:
Los archivos CSV surgieron en la década de 1970 como una forma de facilitar la transferencia de datos entre programas diferentes. El formato ha evolucionado a lo largo de los años y se ha convertido en una herramienta popular entre los programadores.

Una alternativa al formato CSV es el formato TSV (tab-separated values), que utiliza tabulaciones en lugar de comas para separar los datos. Esto puede ser útil en casos donde se necesitan comas en los datos mismos.

Para trabajar con CSV en Rust, se puede utilizar la biblioteca oficial de Rust llamada "csv". Esta biblioteca tiene una documentación detallada y ofrece una variedad de funciones útiles para trabajar con archivos CSV.

## Ver también:
- [Documentación de la biblioteca CSV de Rust](https://docs.rs/csv)
- [Guía para trabajar con archivos CSV en Rust](https://www.techiedelight.com/working-with-csv-files-rust/)