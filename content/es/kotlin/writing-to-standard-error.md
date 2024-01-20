---
title:                "Escreviendo a error estándar"
html_title:           "Kotlin: Escreviendo a error estándar"
simple_title:         "Escreviendo a error estándar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Qué & Por qué?

Escribir a la salida de error estándar es una práctica común entre los programadores. Consiste en enviar mensajes de error o información adicional a la consola de la terminal mientras se ejecuta un programa. Esto se hace para facilitar la depuración y el monitoreo del código durante la ejecución.

Los programadores utilizan la salida de error estándar para mostrar mensajes de error cuando el código encuentra un error en tiempo de ejecución. También se puede utilizar para imprimir información adicional que puede ser útil para entender la causa de un error o el comportamiento del programa.

## Cómo:

```Kotlin
fun main() {
    val num = 0
    try {
        val result = 10 / num
    } catch (e: ArithmeticException) {
        System.err.println("¡No se puede dividir entre cero!")
    }
}
```

Output: No se puede dividir entre cero!

En el ejemplo anterior, el mensaje de error se imprime en la salida de error estándar, lo que permite al programador identificar el error y tomar medidas para solucionarlo.

## Inmersión Profunda:

La práctica de escribir a la salida de error estándar es común en varios lenguajes de programación, incluyendo Kotlin. Sin embargo, algunos lenguajes como Java tienen la opción de escribir mensajes de error a la salida estándar utilizando la función `e.printStackTrace()`. La diferencia es que al utilizar `System.err.println()`, el mensaje se mostrará con un color diferente en la consola, lo que lo hace más fácil de distinguir.

Además, también es posible redirigir la salida de error estándar a un archivo, utilizando el operador `>` en la línea de comandos al ejecutar el programa.

En cuanto a la implementación, escribir a la salida de error estándar se logra utilizando la clase `System` y su propiedad `err`. A través de esta propiedad, se puede acceder al flujo de la salida de error estándar y escribir mensajes utilizando el método `println()`.