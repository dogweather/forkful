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

## Por qué

¿Alguna vez te has preguntado si un directorio existe antes de intentar acceder a él? Ya sea para evitar posibles errores o para tomar decisiones basadas en su existencia, comprobar si un directorio existe es una práctica común en la programación. En este artículo, aprenderemos cómo realizar esta verificación utilizando Kotlin.

## Cómo hacerlo

```Kotlin
fun main() {
    val directory = File("ruta/a/directorio")
    if (directory.exists()) {
        // Realiza acciones si el directorio existe
    } else {
        // Realiza acciones si el directorio no existe
    }
}
```

El ejemplo anterior utiliza el método `exists()` de la clase `File` para determinar si el directorio especificado existe o no. Este método devuelve un booleano que indica si el directorio existe o no. Si el directorio existe, se puede acceder a él y ejecutar las acciones necesarias.

Otra forma de verificar la existencia de un directorio es utilizando el método `isDirectory()` de la clase `File`. Este método también devolverá un booleano, pero solo se considerará verdadero si el directorio especificado existe y es un directorio, no un archivo.

```Kotlin
fun main() {
    val directory = File("ruta/a/directorio")
    if (directory.isDirectory()) {
        // Realiza acciones si el directorio existe y es un directorio
    } else {
        // Realiza acciones si el directorio no existe o no es un directorio
    }
}
```

Si se necesita realizar acciones más específicas en función de si el directorio existe o no, también se puede utilizar la función `let()` junto con el método `exists()`. Esto permitirá ejecutar diferentes acciones dependiendo del resultado.

```Kotlin
fun main() {
    val directory = File("ruta/a/directorio")
    directory.exists().let { exists ->
        if (exists) {
            // Realiza acciones si el directorio existe
        } else {
            // Realiza acciones si el directorio no existe
        }
    }
}
```

## Profundizando

Al utilizar los métodos `exists()` e `isDirectory()` para comprobar la existencia de un directorio, es importante tener en cuenta que la respuesta puede variar en función del sistema operativo en el que se esté ejecutando el código. Por ejemplo, en Windows se utiliza `\` como separador de directorios, mientras que en Mac o Linux se utiliza `/` como separador.

También es importante tener en cuenta que en algunos casos, el método `exists()` puede devolver un valor falso si el directorio no se puede acceder por problemas de permisos o si hay algún error al intentar acceder a él. Por lo tanto, es importante tener un manejo de esas excepciones en caso de que se presenten.

## Ver también

- [Documentación oficial de Kotlin](https://kotlinlang.org/docs/reference/)
- [Guía de estilo de Kotlin](https://kotlinlang.org/docs/reference/coding-conventions.html)
- [Código fuente del proyecto Kotlin en GitHub](https://github.com/JetBrains/kotlin)