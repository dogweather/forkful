---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con CSV implica leer y escribir en un formato de texto simple que representa datos en tablas. Lo usamos porque es universal, fácil de entender y compatible con la mayoría de las herramientas de hoja de cálculo.

## Cómo hacerlo:
```Go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
    "strings"
)

func main() {
    // Escribir CSV
    records := [][]string{
        {"nombre", "edad", "ciudad"},
        {"Alice", "25", "Madrid"},
        {"Bob", "30", "Barcelona"},
    }

    file, err := os.Create("personas.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    for _, record := range records {
        if err := writer.Write(record); err != nil {
            panic(err)
        }
    }

    // Leer CSV
    csvData := `
nombre,edad,ciudad
Alice,25,Madrid
Bob,30,Barcelona
`
    reader := csv.NewReader(strings.NewReader(csvData))

    for {
        record, err := reader.Read()
        if err != nil {
            break
        }
        fmt.Println(record)
    }
}
```

Salida de muestra al leer:
```
[nombre edad ciudad]
[Alice 25 Madrid]
[Bob 30 Barcelona]
```

## Profundización:
Históricamente, CSV (valores separados por comas) se utiliza desde hace décadas para importar y exportar datos de una aplicación a otra. Los alternativos incluyen JSON, XML y bases de datos, pero CSV permanece por su simplicidad. Técnicamente, usar CSV en Go es directo gracias al paquete `encoding/csv` que maneja comillas, saltos de línea y otros casos complicados.

## Ver También:
- Documentación oficial de Go para el paquete `csv`: https://pkg.go.dev/encoding/csv
- Tutorial de Go sobre leer archivos CSV: https://golangbot.com/read-csv/
- Comparación de formatos de serialización de datos (incluyendo CSV, JSON, XML): https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
