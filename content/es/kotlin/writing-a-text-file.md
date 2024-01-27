---
title:                "Escritura de un archivo de texto"
date:                  2024-01-19
html_title:           "Bash: Escritura de un archivo de texto"
simple_title:         "Escritura de un archivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir un archivo de texto implica guardar datos en un documento legible. Los programadores hacen esto para almacenar configuraciones, resultados de programas, o para intercambiar información entre diferentes partes de un programa o con otros programas.

## Cómo hacerlo:
Para escribir un archivo de texto en Kotlin, puedes usar la función `writeText` que ofrece la librería estándar. Aquí hay un ejemplo:

```Kotlin
import java.io.File

fun main() {
    val mensaje = "¡Hola, Kotlin!"
    File("saludo.txt").writeText(mensaje)
}
```

Tras ejecutar el código, encontrarás un archivo `saludo.txt` con el contenido "¡Hola, Kotlin!".

Otro método es usar `printWriter` para escribir línea por línea:

```Kotlin
import java.io.File

fun main() {
    File("saludo_lineas.txt").printWriter().use { out ->
        out.println("Primera línea")
        out.println("Segunda línea")
        out.println("Tercera línea")
    }
}
```

Este código crea un archivo `saludo_lineas.txt` con tres líneas de texto.

## A Fondo
La escritura de archivos en discos se remonta a los inicios de la informática y ha evolucionado con los sistemas operativos. En Kotlin, `writeText` y `printWriter` son dos formas sencillas de manejar archivos, pero hay otras como `FileOutputStream` para mayor control o `Files` con la API NIO para operaciones más avanzadas.

Además, puedes agregar un modo de apertura en `FileWriter` (por ejemplo, para agregar texto sin sobrescribir el archivo) y control de excepciones con `try...catch` para manejar errores al escribir en archivos.

## Ver También
- [Oracle Java Documentation - java.io.PrintWriter](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/PrintWriter.html)
- [Oracle Java Documentation - Working with Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
