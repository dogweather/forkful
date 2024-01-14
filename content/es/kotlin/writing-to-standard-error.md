---
title:    "Kotlin: Escribiendo en el error estándar"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## ¿Por qué escribir a la salida de errores en Kotlin?

Hay varias razones por las que un programador querría escribir a la salida estándar de errores en Kotlin. Una de ellas es para depurar y solucionar problemas en su código, ya que al escribir a la salida estándar de errores se pueden ver los errores y excepciones que ocurran en tiempo de ejecución. Esto permite al programador identificar y corregir posibles problemas en su código.

Otra razón para escribir a la salida estándar de errores es para generar logs y registros de actividades. Al escribir a la salida estándar de errores, se pueden registrar eventos importantes en la ejecución de un programa, lo que puede ser útil para rastrear errores o para tener un registro de las acciones realizadas en el código.

## Cómo escribir a la salida de errores en Kotlin

Es muy sencillo escribir a la salida estándar de errores en Kotlin utilizando la función `System.err.println()`. Esta función toma un argumento de tipo `String` y lo imprime en la salida estándar de errores.

Para utilizar esta función en Kotlin, simplemente escriba `System.err.println()` seguido del texto que desea imprimir entre paréntesis. Por ejemplo:

```Kotlin
fun main() {
    System.err.println("Este es un mensaje de error")
}
```

La salida de este código se vería así:

```Kotlin
Este es un mensaje de error
```

## Profundizando en la escritura a la salida de errores en Kotlin

Además de la función `System.err.println()`, también se puede utilizar la propiedad `error` del objeto `System.out` para escribir a la salida estándar de errores. Esta propiedad devuelve un flujo de salida que se puede utilizar para imprimir mensajes de error. Un ejemplo de cómo utilizarlo sería:

```Kotlin
fun main() {
    System.err.error.println("Mensaje de error utilizando la propiedad 'error'")
}
```

La salida de este código sería la misma que en el ejemplo anterior.

También se puede personalizar la salida de errores utilizando la clase `PrintWriter`. Esta clase proporciona más opciones para la impresión de mensajes de error, como por ejemplo, la posibilidad de escribir a un archivo o a un flujo de salida diferente. Un ejemplo de cómo utilizar esta clase sería:

```Kotlin
fun main() {
    val printWriter = PrintWriter("archivo.txt")
    printWriter.println("Mensaje de error personalizado")
    printWriter.close()
}
```

La salida de este código sería un archivo llamado "archivo.txt" con el mensaje de error dentro de él.

## Vea también

- [Documentación de Kotlin: System Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-system/index.html)
- [Escribiendo a la Salida Estándar de Errores en Java](https://www.baeldung.com/java-write-to-standard-error)
- [Utilizando PrintWriter en Kotlin](https://stackoverflow.com/questions/37592535/is-there-a-way-to-write-to-the-system-error-output-in-kotlin)