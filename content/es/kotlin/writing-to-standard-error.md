---
title:    "Kotlin: Escribiendo en el error estándar"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir a la salida de error en Kotlin

Escribir a la salida de error es una práctica común en la programación en Kotlin para ayudar en la depuración de código y para mejorar la experiencia del usuario al mostrar mensajes de error más claros.

## Cómo hacerlo

Para escribir a la salida de error, se utiliza la función `System.err.println()`. Esta función tomará una cadena como argumento y escribirá esa cadena en la salida de error. Aquí hay un ejemplo de cómo usarlo:

```Kotlin
fun main() {
    val numero = 10

    if (numero > 5) {
        System.err.println("El número es mayor que 5")
    }
}
```

La salida de este código será:

```
El número es mayor que 5
```

## Profundizando más

Al escribir a la salida de error, es importante tener en cuenta que esta salida se muestra en rojo en la consola, lo que la diferencia de la salida estándar. Esto puede ser útil para identificar rápidamente mensajes de error en un gran volumen de texto. Además, también puede utilizar la función `System.err.print()` si desea escribir la cadena sin un salto de línea al final.

También es importante mencionar que, aunque es una práctica común, escribir a la salida de error debe ser utilizado con moderación y solo para mensajes de error relevantes. No se recomienda usarlo para imprimir información general o mensajes de depuración, ya que esto puede afectar negativamente el rendimiento del código.

## Ver también

- [Documentación de Kotlin sobre salida de error](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/system.err.html)
- [Blog sobre cómo manejar excepciones en Kotlin](https://www.baeldung.com/kotlin-exceptions)
- [Guía de estilo de código de Kotlin](https://kotlinlang.org/docs/reference/coding-conventions.html)