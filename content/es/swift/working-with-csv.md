---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/swift/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Trabajar con CSV (Valores Separados por Comas) es manejar datos en un formato de texto plano, esencial para la importación y exportación de datos en aplicaciones. Los programadores lo usan por su simplicidad y por ser ampliamente soportado en distintos sistemas y lenguajes de programación.

## Cómo hacerlo:

Swift facilita la lectura y escritura de archivos CSV. Aquí tienes un ejemplo de cómo parsear un CSV simple:

```Swift
import Foundation

let csvString = """
nombre,edad,ciudad
Juan,28,Sevilla
Ana,34,Madrid
"""

var rows = csvString.components(separatedBy: "\n")
rows.removeFirst() // Eliminamos el encabezado

for row in rows where !row.isEmpty {
    let columns = row.components(separatedBy: ",")
    print("Nombre: \(columns[0]), Edad: \(columns[1]), Ciudad: \(columns[2])")
}
```

Salida de muestra:
```
Nombre: Juan, Edad: 28, Ciudad: Sevilla
Nombre: Ana, Edad: 34, Ciudad: Madrid
```

Y así es cómo puedes escribir en un CSV:

```Swift
let data = [
    ["nombre", "edad", "ciudad"],
    ["Carlos", "22", "Valencia"],
    ["Lucia", "29", "Barcelona"]
]

let csvData = data.map { row in row.joined(separator: ",") }
                  .joined(separator: "\n")

do {
    try csvData.write(to: URL(fileURLWithPath: "ruta/al/archivo.csv"), atomically: true, encoding: .utf8)
    print("Archivo CSV guardado exitosamente.")
} catch {
    print(error.localizedDescription)
}
```

## Análisis Detallado:

Históricamente, los archivos CSV se han usado desde los primeros días de las computadoras personales. A pesar de la falta de un estándar estricto, su simplicidad lo ha convertido en un recurso atemporal. Alternativas como JSON y XML ofrecen más estructura pero son más complejos. Al trabajar con CSV en Swift, es importante manejar correctamente los encabezados y tener en cuenta los detalles como el manejo de comillas y comas dentro de las celdas.

## Ver También:

Para más detalles y bibliotecas para manejar CSV en Swift, puedes visitar:

- [CSV.swift](https://github.com/yaslab/CSV.swift): Una biblioteca de terceros para el manejo de CSV en Swift.
- [Swift Tutorial - Working with CSV](https://www.hackingwithswift.com/example-code/system/how-to-parse-a-csv-file-using-string): Un tutorial para entender mejor el parsing de CSV. 
- [Apple's Swift Documentation](https://developer.apple.com/documentation/swift): Documentación oficial de Swift para profundizar en el lenguaje.
