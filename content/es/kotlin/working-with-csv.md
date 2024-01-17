---
title:                "Trabajando con csv"
html_title:           "Kotlin: Trabajando con csv"
simple_title:         "Trabajando con csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

##¡Qué y por qué? 
Trabajar con CSV es una forma de manejar y manipular datos en formato de tabla. Puede ser útil para organizar grandes cantidades de información de manera fácilmente legible y accesible. Los programadores a menudo trabajan con CSV para importar y exportar datos entre diferentes aplicaciones y sistemas. 

## ¡Cómo hacerlo! 
Kotlin ofrece una forma sencilla de trabajar con CSV utilizando la librería de la [JavaCSV](https://sourceforge.net/projects/javacsv/). Veamos un ejemplo de cómo leer y escribir en un archivo CSV usando Kotlin.

```Kotlin
val reader = CSVReaderBuilder(FileReader("datos.csv")).build()
val datos = reader.readAll()
val writer = CSVWriter(FileWriter("nuevos_datos.csv"))
writer.writeNext(arrayOf("Nombre", "Edad", "Pais"))
writer.writeAll(datos)
writer.close()
```

El código anterior utiliza la librería JavaCSV para leer los datos de un archivo CSV y almacenarlos en una lista llamada `datos`. Luego, utiliza la misma librería para escribir los datos en un nuevo archivo CSV. Podemos ver que el código es bastante simple y legible, lo que lo hace ideal para trabajar con grandes cantidades de datos.

## Profundizando 
El formato CSV (Comma Separated Values) se ha utilizado desde los años 70 para almacenar y compartir datos en forma de tabla. Aunque se ha vuelto un poco obsoleto con la introducción de otros formatos como JSON, sigue siendo ampliamente utilizado, especialmente para importar y exportar datos en aplicaciones de hojas de cálculo.

Existen algunas alternativas a la librería JavaCSV, como [OpenCSV](http://opencsv.sourceforge.net/) y [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/), pero JavaCSV sigue siendo una opción popular entre los programadores de Kotlin debido a su simplicidad y facilidad de uso.

Si quieres profundizar en la implementación de la librería JavaCSV y aprender más sobre cómo manejar datos CSV en Kotlin, puedes consultar la [documentación oficial](https://sourceforge.net/p/javacsv/wiki/JavaCsvDocumentation/) de la librería.

## Ver también 
- [Documentación oficial de JavaCSV](https://sourceforge.net/p/javacsv/wiki/JavaCsvDocumentation/)
- [Tutorial de manejo de archivos CSV en Kotlin](https://www.youtube.com/watch?v=v5zIbtGhI1c)
- [Ejemplos de proyectos que utilizan JavaCSV](https://sourceforge.net/p/javacsv/wiki/JavaCsvListOfProjects/)