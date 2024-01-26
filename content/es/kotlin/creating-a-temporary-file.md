---
title:                "Creando un archivo temporal"
date:                  2024-01-20T17:41:14.121002-07:00
model:                 gpt-4-1106-preview
simple_title:         "Creando un archivo temporal"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Crear archivos temporales es el proceso de generar ficheros que sólo se necesitan durante la ejecución de un programa. Los programadores los utilizan para almacenar datos transitorios sin afectar la estructura permanente del sistema de archivos. 

## How to:

Kotlin facilita la creación de archivos temporales con funciones incorporadas. Aquí te muestro cómo:

```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    // Crear un archivo temporal en el directorio predeterminado
    val tempFileDefault = Files.createTempFile(null, ".tmp")
    println("Archivo temporal en directorio predeterminado: ${tempFileDefault.toAbsolutePath()}")

    // Crear un archivo temporal en un directorio específico
    val tempDir = Paths.get(System.getProperty("user.home"), "tempDir")
    Files.createDirectories(tempDir) // Aquí te aseguras de que el directorio exista
    val tempFileCustom = Files.createTempFile(tempDir, "miArchivoTemp", ".tmp")
    println("Archivo temporal en directorio personalizado: ${tempFileCustom.toAbsolutePath()}")

    // Borrar los archivos temporales al terminar el programa
    tempFileDefault.toFile().deleteOnExit()
    tempFileCustom.toFile().deleteOnExit()
}
```

Si corres este código, verás dos líneas de salida con las rutas completas de los nuevos archivos temporales.

## Deep Dive

La necesidad de crear archivos temporales se remonta a los días de los sistemas operativos primigenios, donde el aislamiento de datos en uso era crucial para el rendimiento y la seguridad. Hoy en día, con la programación multitarea y en entornos concurrentes, estos archivos son esenciales para prevenir la corrupción de datos.

En Java, la clase `java.io.File` proporcionaba métodos para crear archivos temporales. Kotlin, interoperando con la API de Java, hace uso de `java.nio.file.Files` para una mejor experiencia.

Una alternativa a la creación de archivos temporales es el uso de memoria virtual o “memoria temporaria”; sin embargo, esto no es tan seguro y podría ser menos eficiente si se manejan grandes cantidades de datos.

Detalles de implementación a considerar:
- Kotlin y Java colocan los archivos temporales en una ubicación específica del sistema a menos que se especifique una ruta.
- La función `deleteOnExit()` elimina el archivo cuando termina la máquina virtual de Java, pero no es 100% confiable en entornos donde hay varios proceso. Es mejor implementar una limpieza manual más rigurosa.

## See Also

- La documentación oficial de la clase [`Files`](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html) para más métodos y detalles.
- Para entender cómo Java maneja los archivos temporales y cómo esto se aplica a Kotlin: [Oracle Java Documentation - The `java.io.File` Class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#createTempFile(java.lang.String,%20java.lang.String)).
