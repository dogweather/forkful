---
title:    "Kotlin: Leyendo un archivo de texto"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Por qué

Aprender a leer un archivo de texto en Kotlin es una habilidad útil para cualquier programador, ya que te permitirá acceder y procesar datos almacenados en formato de texto plano. Además, te ayudará a comprender mejor el manejo de archivos en general y a manejar situaciones en las que sea necesario interactuar con archivos de texto.

## Cómo

La lectura de un archivo de texto en Kotlin es algo sencillo de aprender y se puede hacer en pocos pasos. Primero, debes importar la clase "File" de Kotlin, que es la encargada de manejar archivos. Luego, utilizando la función "readText()" de la clase File, puedes leer todo el contenido del archivo de texto y almacenarlo en una variable. Aquí un ejemplo de código:

```
Kotlin val archivoTexto = File("mi_archivo.txt")
val contenido = archivoTexto.readText()
println(contenido)
```

Este código abrirá y leerá el archivo "mi_archivo.txt" y luego imprimirá su contenido en la consola. Si deseas leer el archivo línea por línea, puedes utilizar la función "forEachLine()" de la clase File. Un ejemplo de código sería el siguiente:

```
Kotlin val archivoTexto = File("mi_archivo.txt")
archivoTexto.forEachLine {
    println(it)
}
```

Esta función ejecutará el bloque de código dentro de las llaves para cada línea del archivo. Dentro del bloque, "it" se refiere a cada línea del archivo. Puedes utilizar esta técnica para procesar cada línea del archivo de texto y realizar cualquier operación que necesites.

## Profundizando

Además de estas funciones básicas, la clase File ofrece una gran variedad de métodos para manejar y leer archivos de texto en Kotlin. Puedes aprender más sobre ellos en la documentación oficial de Kotlin sobre la clase File.

También es importante mencionar que no solo puedes leer archivos de texto con la clase File, sino que también puedes escribir en ellos utilizando la función "writeText()". De esta manera, puedes crear y modificar archivos de texto desde tu código.

## Ver también

- [Documentación oficial de Kotlin sobre la clase "File"](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Guía de programación en Kotlin](https://developer.android.com/kotlin/learn)
- [Artículo sobre el manejo de archivos en Kotlin](https://www.geeksforgeeks.org/kotlin-file-class-readwrite-files/)