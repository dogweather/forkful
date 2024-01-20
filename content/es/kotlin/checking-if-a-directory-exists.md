---
title:                "Comprobando si existe un directorio"
html_title:           "Kotlin: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Comprobando si un directorio existe en Kotlin

## ¿Qué y Por qué?
Verificar si un directorio existe es un procedimiento habitual que permite confirmar la presencia de un directorio antes de trabajar con él. Los programadores lo hacen para prevenir errores al interactuar con directorios inexistentes.

## ¿Cómo hacerlo?

Aquí te enseñaré lo fácil que es verificar si un directorio existe en Kotlin. Gracias al paquete `java.nio.file` de Java, lo puedes hacer con solo una línea de código.

```kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val dirPath = Paths.get("/direccion/al/directorio")
    val exists = Files.exists(dirPath)
    
    println(exists)
}
```

Este código imprime `true` si el directorio existe y `false` si no existe.

## Profundizando

Historia: Kotlin es un lenguaje relativamente nuevo, pero al ser interoperable con Java, puede utilizar las bibliotecas y paquetes de este último.

Alternativas: Una opción alternativa para verificar si un directorio existe en Kotlin sería utilizando la clase `File` del paquete `java.io`.

```kotlin
import java.io.File

fun main() {
    val dir = File("/direccion/al/directorio")
    val exists = dir.exists() && dir.isDirectory

    println(exists)
}
```

Implementación: `Files.exists(path)` verifica si un archivo existe o no, sin especificar si se trata de un archivo o un directorio. Para asegurarse de que es un directorio, se utiliza `dir.isDirectory` después de comprobar la existencia del archivo.

## Ver también

Para más información, puedes revisar los siguientes enlaces:

1. [Documentación de Java NIO File](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html) - Documentación oficial de Oracle sobre el paquete java.nio.file.
2. [Documentación oficial de Kotlin](https://kotlinlang.org/docs/home.html) - Información detallada y ejemplos adicionales de Kotlin.
3. [Java IO File en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html) - Más usos de la clase File de Java IO en Kotlin.