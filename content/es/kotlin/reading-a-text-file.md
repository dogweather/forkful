---
title:                "Leer un archivo de texto"
html_title:           "Kotlin: Leer un archivo de texto"
simple_title:         "Leer un archivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Qué & Por qué?

Leer un archivo de texto es una tarea común en la programación que implica acceder al contenido de un archivo almacenado en la computadora. Los programadores lo hacen para obtener datos o información específica almacenada en un archivo de texto.

## Cómo hacerlo:

Para leer un archivo de texto en Kotlin, podemos usar la función ```readText()``` que toma una ruta de archivo como parámetro y devuelve una cadena con el contenido del archivo. Por ejemplo:

```Kotlin
val file = "ruta/del/archivo.txt"
val content = file.readText()
println(content)
```

La salida será el contenido del archivo de texto en la consola.

## Profundizando:

Leer un archivo de texto es una habilidad importante en la programación, ya que permite a los programadores acceder a datos almacenados de manera estructurada. Antes de la era de las computadoras, los archivos de texto se utilizaban para almacenar datos. Actualmente, existen muchas alternativas a leer archivos de texto, como bases de datos o archivos CSV. Sin embargo, leer archivos de texto sigue siendo una tarea necesaria en muchas aplicaciones y puede ser una buena opción para almacenar datos simples.

## Vea también:

- [Documentación oficial de Kotlin sobre archivos de texto](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Artículo sobre lectura y escritura de archivos en Kotlin](https://www.tutorialkart.com/kotlin/kotlin-read-text-file/)
- [Otra alternativa para leer archivos de texto en Kotlin](https://www.educative.io/edpresso/how-to-read-a-text-file-using-kotlin)