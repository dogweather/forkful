---
title:    "Kotlin: Comprobando si existe un directorio"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por qué
Antes de empezar a programar, es importante asegurarse de tener toda la información necesaria para que nuestro código funcione correctamente. Parte de esto puede ser comprobar si un directorio existe antes de realizar operaciones en él. En esta entrada, veremos cómo hacerlo en Kotlin.

## Cómo hacerlo
Para comprobar si existe un directorio, podemos utilizar la función `exists()` de la clase `File`. Primero, importamos la clase `java.io.File`:

```Kotlin
import java.io.File
```

Luego, creamos una instancia de `File` con el nombre del directorio que queremos comprobar:

```Kotlin
val directorio = File("mi_directorio")
```

Finalmente, utilizamos la función `exists()` para verificar si el directorio existe:

```Kotlin
if (directorio.exists()) {
    println("El directorio existe!")
} else {
    println("El directorio no existe :(")
}
```

Si el directorio existe, nuestro código imprimirá "El directorio existe!" en la consola. De lo contrario, imprimirá "El directorio no existe :(".

## Profundizando
Si queremos obtener más información sobre el directorio que estamos comprobando, podemos utilizar otras funciones de la clase `File`. Algunas opciones son `isDirectory()`, que nos dice si el archivo es un directorio o no, y `listFiles()`, que nos devuelve un array con todos los archivos dentro del directorio.

Además, también podemos utilizar el objeto `Path` para realizar estas mismas comprobaciones. Primero, importamos la clase `java.nio.file.Path`:

```Kotlin
import java.nio.file.Path
```

Luego, creamos una instancia de `Path` con el nombre del directorio que queremos comprobar:

```Kotlin
val path = Path.of("mi_directorio")
```

Finalmente, utilizamos la función `exists()` del objeto `Files` para verificar si el directorio existe:

```Kotlin
if (Files.exists(path)) {
    println("El directorio existe!")
} else {
    println("El directorio no existe :(")
}
```

## Ver también
- [Documentación de la clase File en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html)
- [Documentación de la clase Path y el objeto Files en Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.nio.file.-path/index.html)