---
title:                "Kotlin: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por qué crear un archivo temporal en Kotlin

A menudo, al desarrollar una aplicación en Kotlin, nos encontramos en la necesidad de crear archivos temporales. Estos archivos son útiles para almacenar datos de forma temporal antes de ser guardados permanentemente o para realizar procesos que requieren la creación de un archivo temporal. En esta entrada, te enseñaré cómo puedes crear un archivo temporal en Kotlin y profundizar un poco más en el tema.

## Cómo crear un archivo temporal en Kotlin

Para crear un archivo temporal en Kotlin, utilizamos la clase `java.io.File` y el método `createTempFile()`. Este método acepta dos parámetros: el nombre del archivo y la extensión. A continuación, se muestra un ejemplo de cómo crear un archivo temporal llamado "temp" con la extensión ".txt":

```Kotlin
val file = File.createTempFile("temp", ".txt")
```

Con esta línea de código, hemos creado un archivo temporal en la ruta predeterminada del sistema. Sin embargo, podemos especificar una ubicación utilizando el siguiente código:

```Kotlin
val file = File("ruta") // Ruta de la ubicación donde queremos crear el archivo 
val tempFile = File.createTempFile("temp", ".txt", file) // Se especifica la ubicación en la que se creará el archivo temporal 
```

Ahora que hemos creado nuestro archivo temporal, podemos realizar diversas operaciones, como escribir o leer datos en él. Veamos un ejemplo de cómo escribir datos en el archivo:

```Kotlin
val file = File.createTempFile("temp", ".txt")
file.writeText("Este es un archivo temporal")
```

En este caso, hemos utilizado el método `writeText()` para escribir el texto "Este es un archivo temporal" en nuestro archivo.

## Profundizando en la creación de archivos temporales en Kotlin

Además de los métodos mencionados anteriormente, `createTempFile()` también tiene más opciones para personalizar la creación de archivos temporales. Por ejemplo, podemos especificar un prefijo y un sufijo para el nombre del archivo de la siguiente manera:

```Kotlin
val file = File.createTempFile("prefijo", "sufijo")
```

También podemos controlar la eliminación automática del archivo temporal utilizando el método `deleteOnExit()` de la clase `java.io.File`. Si llamamos a este método en nuestro archivo, se eliminará automáticamente al finalizar la ejecución de nuestro programa:

```Kotlin
val file = File.createTempFile("temp", ".txt")
file.deleteOnExit()
```

Otra opción es especificar una carpeta donde se almacenarán todos los archivos temporales creados por nuestro programa utilizando la propiedad `java.io.tmpdir`:

```Kotlin
System.setProperty("java.io.tempdir", "ruta") // Ruta de la carpeta donde se almacenarán los archivos temporales
```

Este es solo un breve resumen de cómo crear archivos temporales en Kotlin. Espero que esta información te sea útil en tus proyectos.

## Ver también

- [Documentación oficial de Kotlin sobre la clase File](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Ejemplo de creación de archivos temporales en Kotlin](https://medium.com/@dekinci3/kotlins-file-class-d4626ea8f5c3/)