---
title:                "Trabajando con archivos csv"
html_title:           "Kotlin: Trabajando con archivos csv"
simple_title:         "Trabajando con archivos csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Por qué

Si estás buscando una forma rápida y fácil de manipular y leer datos tabulares, entonces trabajar con archivos CSV puede ser la solución perfecta. CSV, o Comma Separated Values, es un formato comúnmente utilizado para almacenar datos tabulares en una forma sencilla y legible por máquina. Esto lo hace ideal para manejar grandes cantidades de datos y realizar operaciones como filtrar, ordenar y analizar.

## Cómo hacerlo

Para trabajar con archivos CSV en Kotlin, primero debes importar la librería `kotlinx-io-csv` en tu proyecto. Luego, puedes utilizar la función `reader` para leer el archivo CSV y almacenar los datos en una variable. A continuación, puedes iterar sobre los datos y utilizar la función `get` para obtener los valores de cada columna.

```Kotlin
val csv = File("datos.csv").reader().readLines()
for (line in csv) {
    val valores = line.split(",")
    val columna1 = valores.get(0)
    val columna2 = valores.get(1)
    println("Columna 1: $columna1, Columna 2: $columna2")
}
```

Si deseas escribir datos en un archivo CSV, puedes utilizar la función `writer` y escribir cada valor separado por comas utilizando la función `append`.

```Kotlin
val nuevosDatos = listOf("valor1", "valor2", "valor3")
File("datosNuevos.csv").writer().use {
    for (dato in nuevosDatos) {
        it.append("$dato,")
    }
}
```

## Detalles avanzados

Si deseas trabajar con CSV de manera más avanzada, puedes utilizar la librería `opencsv` que ofrece diversas funcionalidades como la posibilidad de especificar el delimitador utilizado en el archivo CSV, manejar datos con comillas, entre otros.

Además, puedes utilizar la función `map` para mapear los datos a una clase personalizada y trabajar con ellos de manera más organizada y estructurada.

## Ver también

- Documentación oficial de Kotlin para trabajar con CSV: https://kotlinlang.org/docs/working-with-csv.html
- Librería kotlinx-io-csv: https://github.com/Kotlin/kotlinx-io/tree/master/csv
- Librería opencsv: http://opencsv.sourceforge.net/