---
title:                "Swift: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-csv.md"
---

{{< edit_this_page >}}

##¿Por qué trabajar con CSV?

CSV (Comma Separated Values) es un formato de archivo utilizado comúnmente para almacenar datos tabulares en una forma que pueda ser fácilmente leída por programas y aplicaciones. Trabajar con archivos CSV puede ser útil para leer y escribir datos en aplicaciones web, análisis de datos, y mucho más.

## Cómo trabajar con CSV

Para trabajar con archivos CSV en Swift, puedes utilizar la librería `CSV` de Swift Package Manager. Primero, importa la librería en tu proyecto:

```Swift
import CSV
```

Luego, puedes leer un archivo CSV y obtener sus datos en una matriz de diccionarios de la siguiente manera:

```Swift
let csv = try! CSV(url: URL(string: "ruta/al/archivo.csv")!)
let rows = csv.namedRows

// Recorrer la matriz y obtener datos
for row in rows {
  print(row["columna1"])
  print(row["columna2"])
  // ...
}
```

También puedes escribir datos en un archivo CSV utilizando la función `write` y especificando los datos como un arreglo de diccionarios:

```Swift
let csv = try! CSV(url: URL(string: "ruta/al/archivo.csv")!)
let data: [[String: String]] = [
    ["producto": "iPhone", "precio": "999"],
    ["producto": "Macbook Pro", "precio": "1999"]
]
try! csv.write(rows: data, delimiter: ",", encoding: .utf8)
```

## Profundizando en el trabajo con CSV

Además de leer y escribir archivos CSV básicos, la librería `CSV` también ofrece características avanzadas como la capacidad de especificar el delimitador, el carácter de cita y la codificación. Puedes explorar más sobre estas características en la documentación oficial de la librería.

## Ver también

- Documentación de la librería `CSV`: https://github.com/yaslab/CSV.swift
- Ejemplos de código: https://github.com/yaslab/SwiftCSV/tree/master/Examples
- Tutorial de trabajo con CSV en Swift: https://www.raywenderlich.com/157128/working-csv-files-swift