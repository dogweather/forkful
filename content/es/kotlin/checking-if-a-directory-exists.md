---
title:                "Kotlin: Comprobando si existe un directorio"
simple_title:         "Comprobando si existe un directorio"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## ¿Por qué comprobar si existe un directorio?

Comprobar si un directorio existe es una tarea importante en la programación Kotlin. Esto permite a los programadores asegurarse de que sus aplicaciones puedan acceder y manipular los archivos y carpetas necesarios para su correcto funcionamiento. Además, esta verificación también puede ayudar a evitar errores y excepciones en el código.

## Cómo hacerlo

¡Afortunadamente, comprobar si un directorio existe es muy sencillo en Kotlin! A continuación, se muestra un ejemplo de código que demuestra cómo hacerlo:

```Kotlin
val directory = File("ruta_del_directorio")
if (directory.exists()) {
    println("El directorio existe")
} else {
    println("El directorio no existe")
}
```

En este código, primero se crea una variable que almacena la ruta del directorio que se desea comprobar. Luego, se utiliza el método `exists()` en esta variable para determinar si el directorio existe o no. Dependiendo del resultado, se imprimirá un mensaje correspondiente.

## Profundizando

El método `exists()` utilizado en el ejemplo anterior es parte de la clase `File` en Kotlin. Esta clase se utiliza para representar archivos y directorios en el sistema de archivos del dispositivo. El método `exists()` devuelve un valor booleano, `true` si el directorio existe y `false` si no existe.

También es importante tener en cuenta que el método `exists()` solo comprueba la existencia del directorio en el momento en que se llama. Si el directorio se crea o se elimina después de que el método se haya ejecutado, el resultado puede ser diferente. Es posible utilizar otros métodos de la clase `File` para obtener información más precisa sobre el directorio, como su tamaño o fecha de creación.

## Ver también

- Documentación oficial sobre la clase `File` en Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Cómo trabajar con archivos y directorios en Kotlin: https://www.baeldung.com/kotlin/files