---
title:                "Kotlin: Creando un archivo temporal"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Por qué

Crear archivos temporales es una práctica común en la programación que puede ser útil por diversas razones. Algunas de las razones más comunes incluyen:

- Probar una funcionalidad antes de integrarla completamente en el código.
- Guardar datos temporales que no son necesarios en la base de datos.
- Optimizar el rendimiento del código al evitar el uso de recursos adicionales.

# Cómo hacerlo

En Kotlin, crear un archivo temporal es muy fácil gracias a la función `createTempFile()`. Esta función toma como parámetros el nombre y la extensión del archivo temporal, y devuelve un objeto `File` que representa el archivo recién creado. Veamos un ejemplo de cómo utilizar esta función:

```Kotlin
fun main() {
    val tempFile = createTempFile("temp", ".txt")
    println("El archivo temporal se ha creado en la siguiente ruta: ${tempFile.absolutePath}")
}
```

En este código, estamos creando un archivo temporal con el nombre "temp" y la extensión ".txt". Si ejecutas este código, verás que se creará un archivo en la ruta predeterminada del sistema (que puede variar según el sistema operativo). Además, el código imprime la ruta absoluta del archivo recién creado.

También puedes especificar una ruta personalizada utilizando el parámetro `directory` de la función `createTempFile()`, como se muestra en el siguiente ejemplo:

```Kotlin
fun main() {
    val tempFile = createTempFile("temp", ".txt", File("/path/to/custom/directory"))
    println("El archivo temporal se ha creado en la siguiente ruta: ${tempFile.absolutePath}")
}
```

# Profundizando

La función `createTempFile()` realiza varias tareas detrás de escena para crear un archivo temporal:

1. Crea un archivo con un nombre único utilizando el prefijo y sufijo proporcionados.
2. Crea el archivo en el directorio temporal predeterminado o en la ruta especificada por el parámetro `directory`.
3. Marca el archivo para que se elimine una vez que el programa finalice su ejecución.

Además, puedes usar el parámetro `prefix` y `suffix` para personalizar el nombre del archivo temporal creado. Si no se especifica ningún prefijo o sufijo, el archivo se nombrará de manera aleatoria.

# Ver también

- [Documentación oficial de Kotlin sobre la función `createTempFile()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/create-temp-file.html)
- [Post de blog sobre cómo manejar archivos en Kotlin](https://www.baeldung.com/kotlin/file-handling)
- [Ejemplo de uso de archivos temporales en aplicaciones de Android](https://developer.android.com/training/data-storage/files/temporary)