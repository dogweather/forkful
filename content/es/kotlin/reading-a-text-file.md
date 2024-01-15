---
title:                "Leyendo un archivo de texto"
html_title:           "Kotlin: Leyendo un archivo de texto"
simple_title:         "Leyendo un archivo de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por qué
Los archivos de texto son una de las formas más comunes de almacenar y leer datos en un programa. Si estás aprendiendo a programar en Kotlin, es importante saber cómo leer archivos de texto para poder trabajar con ellos en tus proyectos. En este artículo, te enseñaré cómo hacerlo de manera sencilla y eficiente.

## Cómo hacerlo
Para leer un archivo de texto en Kotlin, primero necesitas abrirlo y luego leer su contenido. Esto puede hacerse en unas pocas líneas de código, como se muestra a continuación:

```Kotlin
val archivo = File("texto.txt")
val contenido = archivo.readText()
println(contenido)
```

En este ejemplo, estamos abriendo un archivo llamado "texto.txt" y almacenando su contenido en la variable "contenido". Luego, imprimimos ese contenido en la consola. Es importante recordar que debes manejar cualquier posible excepción al abrir o leer el archivo.

## Deep Dive
Si deseas leer un archivo línea por línea, puedes usar el método "forEachLine" en lugar de "readText". Además, si el archivo de texto contiene datos separados por comas u otro delimitador, puedes usar la función "readLines" para leer cada línea y convertirla en una lista de elementos separados.

También hay una forma más avanzada de leer archivos de texto en Kotlin, usando BufferedReader y FileReader. Esta opción es más útil en casos en los que el archivo es demasiado grande para almacenarlo en memoria o si deseas leer solo una parte del archivo.

## Ver También
- [Documentación oficial de lectura de archivos en Kotlin] (https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Guía detallada sobre cómo leer archivos en Kotlin] (https://kotlinlang.org/docs/tutorials/reading-files.html)
- [Ejemplos prácticos de lectura de archivos en Kotlin] (https://www.programiz.com/kotlin-programming/file-handling)