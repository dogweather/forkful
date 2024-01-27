---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Trabajar con archivos CSV implica gestionar datos en un formato de texto simple, organizado por filas y comas. Los programadores lo hacen para manipular grandes cantidades de datos fácilmente, intercambiar información entre sistemas o importar/exportar desde hojas de cálculo y bases de datos.

## Cómo hacerlo:
```Gleam
import gleam/csv
import gleam/io

pub fn main() -> Result(Nil, String) {
  let data = "nombre,edad,ciudad\nJuan,34,Madrid\nLucia,28,Barcelona"
  let rows = csv.decode(data)
  
  case rows {
    Ok(records) ->
      io.println("Lectura exitosa!")
      records
      |> List.map(fn(row) { row.fields })
      |> io.debug
      Err(_error) ->
        io.println("Error al leer CSV")
  }
}
```
Salida de ejemplo:
```
Lectura exitosa!
[[nombre, edad, ciudad], [Juan, 34, Madrid], [Lucia, 28, Barcelona]]
```

## Estudio detallado
El formato CSV se remonta a principios de los años 70 y se ha convertido en un estándar de facto para el intercambio de datos tabulares. Alternativas como JSON o XML ofrecen estructuras más complejas y meta-datos, pero CSV destaca por su simplicidad y amplia aceptación. En Gleam, trabajar con CSV es directo gracias al módulo `gleam/csv`, el cual ofrece funciones para decodificar y codificar datos CSV.

## Ver También
- [RFC 4180, el estándar que define CSV](https://tools.ietf.org/html/rfc4180)
- [Comunidad Gleam](https://gleam.run/community/)
