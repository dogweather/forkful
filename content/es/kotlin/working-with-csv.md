---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
html_title:           "Bash: Trabajando con archivos CSV"
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué & Por Qué?

Trabajar con CSV significa manipular datos en un formato de texto simple que representa tablas. Los programadores lo hacen porque es un estándar ligero y compatible con muchas herramientas, ideal para importar o exportar datos sencillos.

## Cómo hacerlo:

```kotlin
import java.io.File

fun main() {
    val filePath = "datos.csv"
    val csvData = File(filePath).readLines()

    csvData.forEach { line ->
        val columns = line.split(",")
        println("Nombre: ${columns[0]}, Edad: ${columns[1]}")
    }
}
```

Resultado:

```
Nombre: Juan, Edad: 30
Nombre: Ana, Edad: 25
```

## Inmersión Profunda

CSV, siglas de Comma-Separated Values, se origina a principios de los años 70. Alternativas incluyen JSON y XML, que son más ricos en características pero más pesados. Cuando trabajas con CSV en Kotlin, puedes optar por bibliotecas como OpenCSV o simplemente usar la librería estándar si tus necesidades son básicas.

## Ver También

- Documentación de Kotlin: [https://kotlinlang.org/docs/home.html](https://kotlinlang.org/docs/home.html)
- Sobre CSV en Wikipedia: [https://es.wikipedia.org/wiki/Valores_separados_por_comas](https://es.wikipedia.org/wiki/Valores_separados_por_comas)
- Biblioteca OpenCSV: [http://opencsv.sourceforge.net/](http://opencsv.sourceforge.net/)
