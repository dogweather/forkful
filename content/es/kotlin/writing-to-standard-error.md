---
title:                "Kotlin: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

¿Por qué escribir a la salida de error estándar?

Una de las prácticas comunes en la programación es escribir a la salida de error estándar. Esto nos permite mostrar mensajes de error o información importante al usuario cuando ocurre algún problema en el código. Es una forma útil de depurar y mejorar nuestro código.

## Cómo hacerlo

Para escribir a la salida de error estándar en Kotlin, utilizamos la función `System.err.println()`. Por ejemplo:

```
fun main() {
    System.err.println("¡Error: no se puede dividir por cero!")
    val x = 10 / 0
}
```

En este ejemplo, al intentar dividir 10 por 0, se generará un error y el mensaje "¡Error: no se puede dividir por cero!" se mostrará en la salida de error estándar.

También podemos utilizar el objeto `System.err` para imprimir mensajes de error en un bloque `try-catch`:

```
fun main() {
    try {
        val x = 10 / 0
    } catch (e: Exception) {
        System.err.println("¡Error al dividir por cero!")
    }
}
```

Además, es importante tener en cuenta que la salida de error estándar es independiente de la salida estándar. Por lo tanto, podemos imprimir mensajes de error sin afectar la salida de nuestro programa.

## Inmersión en detalle

En Kotlin, la salida de error estándar es manejada por el objeto `System.err` de la clase `java.lang.System`. Esta clase también proporciona otras funciones útiles para trabajar con la salida de error, como `System.err.print()` y `System.err.printf()`, que se comportan de manera similar a sus equivalentes en Java.

También es posible redirigir la salida de error estándar a un archivo en lugar de la consola, utilizando la clase `System.setErr()`. Esto puede ser especialmente útil en situaciones en las que no tenemos acceso a la consola, como en aplicaciones móviles.

## Ver también

- [Funciones básicas de Kotlin de la documentación oficial](https://kotlinlang.org/docs/tutorials/kotlin-for-py/functions-basic.html)
- [Expresiones y sentencias de control en Kotlin de la documentación oficial](https://kotlinlang.org/docs/tutorials/kotlin-for-py/control-flow.html)
- [Documentación oficial de Java para la clase System](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)