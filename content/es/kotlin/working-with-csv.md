---
title:                "Kotlin: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## ¿Por qué trabajar con CSV en Kotlin?

CSV o Comma Separated Values es un formato de archivo ampliamente utilizado para almacenar datos en forma de tabla. Muchas aplicaciones y sistemas utilizan archivos CSV para importar y exportar datos de forma sencilla. Por lo tanto, trabajar con CSV en Kotlin puede ser una habilidad valiosa para cualquier programador.

## Cómo hacerlo

A continuación, se mostrarán algunos ejemplos de código en Kotlin para trabajar con archivos CSV.

Para leer un archivo CSV y mostrar sus datos en la consola:

```Kotlin
import java.io.File
import com.github.doyaaaaaken.kotlincsv.dsl.csvReader

val file = File("nombre_archivo.csv")

csvReader().open(file) {
    readAllAsSequence().forEach { row ->
        println("${row[0]} | ${row[1]} | ${row[2]}") // Cambiar índices según la estructura del archivo
    }
}
```

El resultado será un listado de cada fila del archivo en la consola con el formato "columna1 | columna2 | columna3".

Para escribir en un archivo CSV:

```Kotlin
import java.io.File
import com.github.doyaaaaaken.kotlincsv.client.CsvWriter

val file = File("nuevo_archivo.csv")

CsvWriter().open(file) {
    writeRow("dato1", "dato2", "dato3") // Reemplazar por los datos a escribir en cada columna
    writeRow("dato4", "dato5", "dato6")
}
```

Con esto, se creará un archivo CSV con los datos especificados en las filas.

## Profundizando

Trabajar con archivos CSV en Kotlin no solo implica leer y escribir datos, sino también manejar errores y formatos de datos específicos. Al trabajar con archivos grandes, puede ser necesario utilizar una librería externa para optimizar el rendimiento. Además, es importante conocer la estructura del archivo CSV (separadores, encabezados, etc.) para poder manipularlo correctamente.

## Ver también

A continuación, se muestran algunos enlaces útiles para profundizar en el trabajo con CSV en Kotlin:

- [Documentación oficial de Kotlin sobre manejo de archivos CSV](https://kotlinlang.org/docs/tutorials/kotlin-for-py/read-write-files.html)
- [Librería kotlincsv para trabajar con CSV en Kotlin](https://github.com/doyaaaaaken/kotlincsv)
- [Ejemplo de código para leer y escribir archivos CSV en Kotlin](https://www.baeldung.com/kotlin-read-write-csv-file)