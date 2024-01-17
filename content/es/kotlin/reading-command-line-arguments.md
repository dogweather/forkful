---
title:                "Leyendo argumentos de línea de comandos."
html_title:           "Kotlin: Leyendo argumentos de línea de comandos."
simple_title:         "Leyendo argumentos de línea de comandos."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# ¿Qué y por qué?

Leer argumentos de línea de comando es una técnica utilizada por los programadores para obtener datos proporcionados por el usuario al ejecutar un programa. Esto les permite personalizar la ejecución del mismo y hacerlo más interactivo para el usuario.

# ¿Cómo hacerlo?

Para leer argumentos de línea de comando en Kotlin, podemos usar la función `args` que se encuentra en la clase `Array<String>`. Aquí hay un ejemplo de código que lee argumentos y muestra un mensaje en la consola con el primer argumento proporcionado:

```
Kotlin fun main(args: Array<String>) {
    println("El primer argumento es: ${args[0]}")
}
```

Si ejecutamos el programa con el siguiente comando: `kotlin Main.kt arg1 arg2`, el resultado sería: `El primer argumento es: arg1`.

# Profundizando

Esta técnica de leer argumentos de línea de comando se utiliza comúnmente desde los primeros días de la programación. En el pasado, los programas solían interactuar directamente con la línea de comandos y los argumentos eran la forma de personalizar la ejecución. Hoy en día, existen alternativas como el uso de archivos de configuración o interfaces gráficas.

La implementación de lectura de argumentos de línea de comando en Kotlin sigue el estándar de Unix y utiliza la convención de `-` para opciones y `--` para opciones extendidas. También es posible acceder a los argumentos a través de su índice en la lista `args`.

# Ver también

- [Kotlin Documentation on Command Line Arguments](https://kotlinlang.org/docs/command-line.html)
- [Java World Article on Reading Command Line Arguments](https://www.javaworld.com/article/3367564/java-programming-without-using-main-methods.html)