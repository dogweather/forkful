---
title:                "Escribiendo en el error estándar"
html_title:           "Arduino: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Escribir en el error estándar permite comunicar errores y diagnósticos sin mezclarlos con la salida de datos normales. Los programadores usan esto para separar los flujos de información y facilitar el debugging y el manejo de errores por parte de sistemas que consumen la salida del programa.

## Cómo Hacerlo:
```kotlin
fun main() {
    // Imprimir en la salida estándar (stdout)
    println("Esto es un mensaje en la salida estándar.")

    // Imprimir en el error estándar (stderr)
    System.err.println("¡Ups! Esto es un error.")
}
```
Salida esperada:
```
Esto es un mensaje en la salida estándar.
¡Ups! Esto es un error.
```

## Análisis Profundo
Históricamente, la diferencia entre salida estándar y error estándar viene de Unix, donde se estableció una convención para separar los flujos de información. Existen alternativas para emitir errores, como el uso de archivos log o frameworks especializados para manejo de errores. La implementación depende del sistema operativo, pero generalmente, la escritura en el error estándar no está sujeta al buffering de salida, lo que significa que los mensajes de error se muestran inmediatamente.

## Ver También
- Documentación oficial de Kotlin: https://kotlinlang.org/docs/reference/
- Unix Standard Streams: https://en.wikipedia.org/wiki/Standard_streams
- Guía de manejo de errores en Kotlin: https://kotlinlang.org/docs/exceptions.html
