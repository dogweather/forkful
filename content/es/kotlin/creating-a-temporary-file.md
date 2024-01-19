---
title:                "Creando un archivo temporal"
html_title:           "Arduino: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Crear un archivo temporal implica la generación de un archivo para uso a corto plazo, normalmente para almacenar datos temporalmente durante la ejecución de un programa. Los programadores lo hacen para manejar grandes cantidades de datos sin consumir demasiada memoria o para guardar información intermedia para tareas de procesamiento por lotes.

## Cómo hacerlo:

Aquí hay un ejemplo de cómo crear un archivo temporal en Kotlin. Primero, importamos la biblioteca de IO y crearemos el archivo utilizando la función `createTempFile`.

```Kotlin
import java.io.File

fun main(){
    val tempFile = File.createTempFile("tempFile", ".temp")
    println("Archivo temporal: ${tempFile.absolutePath}")
}
```

El primer parámetro del método `createTempFile` es el prefijo del nombre del archivo y el segundo es el sufijo (extensión). El resultado esperado seria el camino absoluto al archivo temporal creado.

## Detalles Profundos:

Crear archivos temporales es una práctica común desde hace mucho tiempo en la programación y es habitual en procesos que manejan cantidades enormes de datos. Para Kotlin, el método `createTempFile` es la forma más fácil y segura.

Puedes optar por otras alternativas como el uso de clases de archivo de terceros o la manipulación manual de los archivos pero no se recomienda ya que el método `createTempFile` se encarga de muchas cuestiones de seguridad.

Los archivos temporales se suelen guardar en una ubicación especifica del sistema, pero se puede cambiar ese directorio por defecto proporcionando un tercer parámetro al método `createTempFile` que es el directorio.

```Kotlin
import java.io.File

fun main(){
    val tempDir = File("/ruta/a/mi/directorio")
    val tempFile = File.createTempFile("tempFile", ".temp", tempDir)
    println("Archivo temporal: ${tempFile.absolutePath}")
}
```

## Vea También:

Documentación oficial de Kotlin para la función `createTempFile`: [Aquí](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/create-temp-file.html)

Tutorial de archivos temporales en Java: [Aquí](https://docs.oracle.com/javase/tutorial/essential/io/file.html#createTempFile)