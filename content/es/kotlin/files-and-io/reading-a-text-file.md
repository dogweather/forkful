---
date: 2024-01-20 17:54:52.766937-07:00
description: "Leer un archivo de texto en Kotlin es acceder y manipular el contenido\
  \ de un archivo almacenado en el disco. Los programadores lo hacen para trabajar\
  \ con\u2026"
lastmod: '2024-03-13T22:44:59.053300-06:00'
model: gpt-4-1106-preview
summary: "Leer un archivo de texto en Kotlin es acceder y manipular el contenido de\
  \ un archivo almacenado en el disco. Los programadores lo hacen para trabajar con\u2026"
title: Lectura de un archivo de texto
weight: 22
---

## ¿Qué & Por Qué?

Leer un archivo de texto en Kotlin es acceder y manipular el contenido de un archivo almacenado en el disco. Los programadores lo hacen para trabajar con datos, configuraciones, y más, de una forma fácil y rápida.

## Cómo hacerlo:

### Leer todo el archivo de una vez:

```kotlin
import java.io.File

fun main() {
    val content = File("mi_archivo.txt").readText()
    println(content)
}
```

### Salida de muestra:
```
Hola, este es el contenido de mi archivo de texto.
```

### Leer archivo línea por línea:

```kotlin
import java.io.File

fun main() {
    File("mi_archivo.txt").forEachLine { linea ->
        println(linea)
    }
}
```

### Salida de muestra:
```
Hola,
este es el contenido de mi archivo de texto.
```

### Leer y manejar excepciones:

```kotlin
import java.io.File
import java.io.FileNotFoundException

fun main() {
    try {
        val content = File("mi_archivo.txt").readText()
        println(content)
    } catch (e: FileNotFoundException) {
        println("Archivo no encontrado.")
    }
}
```

### Salida de muestra:
```
Archivo no encontrado.
```

## Deep Dive

Leer archivos de texto es fundamental. En la historia de la programación, diversas formas de hacerlo han evolucionado; cada lenguaje ofrece su conjunto de herramientas. Kotlin, construido sobre la JVM, aprovecha las bibliotecas de Java para facilitar la lectura de archivos, añadiendo simplicidad y manejo de errores mejorado. Alternativas en Kotlin incluyen usar `readLines()` para obtener una lista de líneas, o `BufferedReader` para archivos grandes. La eficiencia importa cuando los archivos son enormes; usar "streaming" reduce el uso de memoria.

## Ver También

- Documentación oficial de Kotlin sobre manejo de archivos (inglés): [Kotlin File Handling](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
