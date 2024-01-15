---
title:                "Creando un archivo temporal"
html_title:           "Kotlin: Creando un archivo temporal"
simple_title:         "Creando un archivo temporal"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por Qué

Si estás trabajando en un proyecto de programación, probablemente te hayas encontrado con la necesidad de crear archivos temporales en algún momento. Un archivo temporal es simplemente un archivo que se utiliza para almacenar datos temporales durante la ejecución de un programa. Esto puede ser útil para realizar pruebas, guardar datos temporales o simplemente para organizar mejor tu código.

## Cómo Hacerlo

Para crear un archivo temporal en Kotlin, podemos utilizar la función `createTempFile()` del paquete `kotlin.io`. Esta función toma como argumentos una cadena que especifica el prefijo del nombre del archivo y otra cadena que especifica la extensión del archivo. Por ejemplo, si queremos crear un archivo temporal llamado "datos_temp.txt" en el directorio actual, podemos hacerlo de la siguiente manera:

```Kotlin
val tempFile = createTempFile("datos_temp", ".txt")
```

Una vez que se llama a esta función, se crea el archivo temporal y se devuelve un objeto `File` que representa ese archivo. Podemos realizar acciones como escribir en el archivo, leer datos de él o eliminarlo al final de nuestra ejecución. A continuación, se muestra un ejemplo de escritura de datos en un archivo temporal y luego su eliminación:

```Kotlin
val tempFile = createTempFile("datos_temp", ".txt")

// Escribir datos en el archivo
tempFile.writeText("Esto es un ejemplo de datos temporales.")

// Eliminar el archivo
tempFile.delete()
```

## Profundizando

Si queremos tener un mayor control sobre la creación de nuestro archivo temporal, también podemos utilizar el constructor `File.createTempFile()` en su lugar. Este constructor toma como argumentos el prefijo, la extensión, un directorio para crear el archivo y un booleano que indica si el archivo debe ser eliminado al finalizar la ejecución. A continuación, se muestra un ejemplo:

```Kotlin
val tempFile = File.createTempFile("datos_temp", ".txt", File("ruta/a/directorio", true))
```

Además, si queremos asegurarnos de que nuestro archivo temporal se elimine incluso si se produce una excepción en la ejecución, podemos utilizar la función `use()` en lugar de llamar a `delete()` manualmente. Esta función automáticamente cierra el flujo de escritura en el archivo y elimina el archivo al final de su uso. A continuación, se muestra un ejemplo:

```Kotlin
File.createTempFile("datos_temp", ".txt").use {
    // Escribir datos en el archivo
    it.writeText("Esto es un ejemplo de datos temporales.")
}
```

## Ver También

- [Documentación sobre la función createTempFile() en la documentación oficial de Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/create-temp-file.html)
- [Artículo sobre la importancia de los archivos temporales en la programación](https://www.drdobbs.com/jvm/temporary-files-in-java-or-the-importanc/228700944)