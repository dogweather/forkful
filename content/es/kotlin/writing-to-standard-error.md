---
title:                "Kotlin: Escribiendo al error estándar"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por qué escribir en el estándar de error en Kotlin

Si eres un desarrollador de Kotlin, es posible que hayas escuchado sobre escribir en el estándar de error mientras depuras tu código. Pero, ¿por qué exactamente deberías hacerlo? En esta publicación de blog, exploraremos las razones por las que es importante escribir en el estándar de error en Kotlin.

## Cómo escribir en el estándar de error en Kotlin

Escribir en el estándar de error en Kotlin es fácil y puede ser de gran ayuda durante el proceso de depuración. Simplemente sigue estos pasos:

```kotlin
fun main() {
    val num = 0
    if (num == 0) {
        System.err.println("¡Error! El número no puede ser igual a 0.")
    }
}
```
Siguiendo el ejemplo anterior, cuando ejecutemos este código, veremos el siguiente resultado en nuestra consola:

```sh
¡Error! El número no puede ser igual a 0.
```

Como puedes ver, al escribir en el estándar de error, podemos mostrar mensajes específicos para ayudarnos a identificar y solucionar errores en nuestro código.

## Profundizando en el estándar de error en Kotlin

Es importante tener en cuenta que el estándar de error es diferente del estándar de salida, también conocido como estándar de impresión. Mientras que el estándar de salida muestra mensajes de información y confirmación, el estándar de error se utiliza para comunicar errores críticos y problemas durante la ejecución del código.

Otra ventaja de escribir en el estándar de error en Kotlin es que los mensajes se mostrarán en un color diferente en la consola, lo que facilita la identificación de errores. Además, al escribir en el estándar de error, podemos añadir detalles adicionales y mensajes de seguimiento para ayudarnos a rastrear y solucionar problemas más complejos.

## Ver también

Si deseas aprender más sobre el estándar de error en Kotlin y cómo puedes utilizarlo en tu código, echa un vistazo a estos recursos adicionales:

- [Documentación oficial de Kotlin sobre el estándar de error](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/-system-err/index.html)
- [Tutorial sobre la depuración en Kotlin utilizando el estándar de error](https://medium.com/@karahasankara/logging-in-kotlin-with-system-out-and-system-err-41800cd6692)
- [Explicación más detallada sobre la diferencia entre estándar de salida y estándar de error en Kotlin](https://www.careerride.com/Kotlin-difference-between-systemoutandSystemerr.aspx)

Esperamos que esta publicación te haya ayudado a comprender mejor por qué es importante escribir en el estándar de error en Kotlin y cómo puedes hacerlo en tus propios proyectos de programación. ¡Happy coding! 💻