---
title:                "Trabajando con archivos CSV"
html_title:           "Go: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/go/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Trabajar con CSV es una tarea común para programadores en Go. CSV, que significa "valores separados por comas" en inglés, es un formato de archivo utilizado para almacenar y manipular datos tabulares. Los programadores utilizan este formato para importar y exportar datos entre diferentes sistemas y aplicaciones.

## ¿Cómo hacerlo?

Empezar a trabajar con CSV en Go es muy sencillo. Solo necesitas importar el paquete "encoding/csv" y luego puedes utilizar las funciones proporcionadas por este paquete para leer y escribir en archivos CSV.

```Go
import "encoding/csv"

// Ejemplo de lectura de un archivo CSV
f, err := os.Open("datos.csv")
if err != nil {
    panic(err)
}

// Utilizar csv.NewReader para leer el archivo
r := csv.NewReader(f)
// El método ReadAll devuelve una matriz con todos los registros del CSV
records, err := r.ReadAll()
if err != nil {
    panic(err)
}
fmt.Println(records)

// Ejemplo de escritura en un archivo CSV
f, err := os.Create("nuevos_datos.csv")
if err != nil {
    panic(err)
}
// Utilizar csv.NewWriter para escribir en un archivo
w := csv.NewWriter(f)
// Escribir un registro en el archivo
w.Write([]string{"Nombre", "Edad", "País"})
// Escribir múltiples registros en el archivo
w.WriteAll([][]string{
    []string{"Juan", "25", "España"},
    []string{"María", "30", "México"},
    []string{"Pedro", "20", "Argentina"},
})
// Importante: llamar al método Flush al final para escribir los cambios en el archivo
w.Flush()
```

## En profundidad

CSV ha sido utilizado en informática desde la década de 1970 y ha evolucionado a lo largo del tiempo. Actualmente, existen varios formatos relacionados con CSV, como TSV (valores separados por tabulaciones) y CSV con encabezados, que incluyen el nombre de cada columna.

Otras alternativas a trabajar con CSV en Go incluyen paquetes como "google.golang.org/csv" y "gonum.org/v1/csv", que ofrecen diferentes funcionalidades y características.

En cuanto a la implementación, el paquete "encoding/csv" utiliza la interfaz "io.Reader" para leer datos y la interfaz "io.Writer" para escribir datos. También proporciona opciones para personalizar el delimitador y el carácter de cita utilizado en los archivos CSV.

## Ver también

- [Documentación oficial de Go sobre el paquete csv](https://golang.org/pkg/encoding/csv/)
- [Especificación oficial del formato CSV](https://tools.ietf.org/html/rfc4180)
- [Opciones de personalización en el paquete "encoding/csv"](https://golang.org/pkg/encoding/csv/#Reader.Comma)