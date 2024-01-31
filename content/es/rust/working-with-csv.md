---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Trabajar con CSV (valores separados por comas) significa lidiar con datos estructurados como texto plano, donde cada línea es un registro y cada registro contiene campos separados por comas. Los programadores usan CSV por su simplicidad y amplia compatibilidad con sistemas de bases de datos, hojas de cálculo y herramientas de análisis.

## Cómo hacerlo:

Para trabajar con CSV en Rust, usaremos la crate `csv`. Primero, añadelo a tu `Cargo.toml`:

```toml
[dependencies]
csv = "1.1"
```

Ahora, puedes leer un archivo CSV así:

```Rust
use csv::Reader;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let mut rdr = Reader::from_path("datos.csv")?;

    for result in rdr.records() {
        let record = result?;
        println!("{:?}", record);
    }
    Ok(())
}
```

Si `datos.csv` contiene:

```
nombre,edad,ciudad
Juan,28,Madrid
Lucia,35,Barcelona
```

La salida será:

```
StringRecord(["Juan", "28", "Madrid"])
StringRecord(["Lucia", "35", "Barcelona"])
```

## Análisis Detallado:

El formato CSV se originó en los años 70 y se ha establecido como un estándar de facto para el intercambio de datos tabulares. Rust ofrece varias alternativas como `serde_csv` para la deserialización con `serde`. A nivel de implementación, es clave manejar correctamente codificación, fin de líneas, y campos que contienen comas, usando comillas, para evitar errores de parsing.

## Ver También:

- Documentación oficial de la crate `csv`: https://docs.rs/csv/latest/csv/
- `serde` y `serde_csv` para deserialización en Rust: https://docs.serde.rs/serde/
- Una guía práctica para manejar CSV desde Rust: https://blog.burntsushi.net/csv/
