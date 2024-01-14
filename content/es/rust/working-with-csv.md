---
title:                "Rust: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué trabajar con CSV en Rust

Trabajar con CSV (Comma-Separated Values) puede ser una tarea común y necesaria para manejar datos en diferentes proyectos de programación. En particular, en el lenguaje de programación Rust, trabajar con CSV puede ser beneficioso ya que ofrece una alta eficiencia y seguridad en el manejo de datos. En este artículo, exploraremos cómo trabajar con CSV en Rust y las diferentes herramientas disponibles para hacerlo de manera efectiva.

## Cómo trabajar con CSV en Rust

Para trabajar con CSV en Rust, necesitaremos importar la biblioteca `csv`. A continuación, podemos crear un archivo CSV utilizando el método `Writer`, especificando las columnas y los datos correspondientes.

````Rust
use csv::Writer;

let mut writer = Writer::from_path("productos.csv")?;

writer.write_record(&["ID", "PRODUCTO", "PRECIO"])?;
writer.write_record(&["1", "Camiseta", "15.99"])?;
writer.write_record(&["2", "Pantalón", "25.99"])?;
writer.write_record(&["3", "Zapatos", "39.99"])?;

````

Una vez que tenemos nuestro archivo CSV creado, podemos leerlo utilizando el método `Reader`, que nos permite iterar sobre cada fila y columna. En el siguiente ejemplo, imprimiremos el producto con el precio más alto.

````Rust
use csv::Reader;

let mut reader = Reader::from_path("productos.csv")?;
let mut precio_mas_alto = 0.0;
let mut producto_mas_caro = "";

for result in reader.records() {
    let record = result?;
    let precio: f64 = record[2].parse()?;
    if precio > precio_mas_alto {
        precio_mas_alto = precio;
        producto_mas_caro = record[1].to_string();
    }
}
println!("El producto más caro es {} con un precio de {}", producto_mas_caro, precio_mas_alto);

````

La salida de este código será:

```
El producto más caro es Zapatos con un precio de 39.99
```

## Profundizando en el manejo de CSV en Rust

Además de las herramientas básicas para trabajar con CSV en Rust, existen algunas bibliotecas y herramientas adicionales que pueden mejorar aún más la eficiencia y seguridad en la manipulación de datos. Algunas de estas son:

- `serde` y `serde_csv`: nos permiten serializar y deserializar directamente desde y hacia archivos CSV.
- `csv-sniffer`: nos ayuda a determinar automáticamente el delimitador y otras características de un archivo CSV.
- `rust-csv-validator`: valida la estructura y contenido de un archivo CSV utilizando configuraciones personalizadas.

## Ver también

- Biblioteca csv en Rust: https://docs.rs/csv/
- Ejemplos de uso de la biblioteca csv: https://github.com/BurntSushi/rust-csv/tree/master/examples.