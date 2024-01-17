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

¡Hola a todos! Hoy vamos a hablar sobre cómo verificar si un directorio existe en Kotlin. Probablemente sepas que los programadores a menudo hacen esto para asegurarse de que su código funcione correctamente.

## ¿Qué y por qué?

Cuando hablamos de verificar si un directorio existe, nos referimos a comprobar si un directorio específico (una carpeta en nuestro sistema de archivos) se encuentra presente o no. Los programadores a menudo hacen esto para evitar que sus programas se bloqueen o para realizar ciertas acciones en caso de que el directorio no exista.

## Cómo hacerlo:

Para comprobar si un directorio existe en Kotlin, podemos utilizar la función `exists()` de la clase `File`. Aquí hay un ejemplo:

```Kotlin
// Importamos la clase File
import java.io.File 

// Creamos una instancia de File con la ruta del directorio que queremos comprobar
val directorio = File("ruta/del/directorio") 

// Llamamos a la función exists() y guardamos el resultado en una variable booleana
val existeDirectorio = directorio.exists() 

// Imprimimos el resultado
println("¿El directorio existe? $existeDirectorio")
```

La salida de este código sería "¿El directorio existe? true" si el directorio existe y "¿El directorio existe? false" si no existe.

## Profundizando:

Si deseamos verificar si un directorio existe sin importar si es un directorio real o un enlace simbólico, podemos utilizar la función `exists()` de la clase `File` en combinación con la función `toRealPath()` para obtener la ruta real del directorio. Esto sería de la siguiente manera:

```Kotlin
// Creamos una instancia de File con la ruta del directorio que queremos comprobar
val directorio = File("ruta/del/directorio") 

// Llamamos a la función exists() en conjunto con toRealPath() y guardamos el resultado en una variable booleana
val existeDirectorio = directorio.exists() || directorio.toRealPath().exists()

// Imprimimos el resultado
println("¿El directorio existe? $existeDirectorio")
```

Otra forma de verificar si un directorio existe es utilizando la función `isDirectory()` en lugar de `exists()`. Esta función solo devolverá verdadero si el objeto `File` representa un directorio. Aquí hay un ejemplo:

```Kotlin
// Creamos una instancia de File con la ruta del directorio que queremos comprobar
val directorio = File("ruta/del/directorio") 

// Llamamos a la función isDirectory() y guardamos el resultado en una variable booleana
val esDirectorio = directorio.isDirectory() 

// Imprimimos el resultado
println("¿El objeto File es un directorio? $esDirectorio")
```

## Ver también:

- Documentación oficial de Kotlin sobre la clase `File`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html
- Artículo en español sobre cómo crear y manipular archivos y directorios en Kotlin: https://medium.com/@waliahimanshu05/creando-y-manipulando-archivos-y-directorios-con-kotlin-2743fb9f5a1c