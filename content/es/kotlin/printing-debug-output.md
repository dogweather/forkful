---
title:                "Kotlin: Imprimiendo salida de depuración"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
A menudo, cuando estamos desarrollando una aplicación, encontramos errores o comportamientos inesperados en nuestro código. Para solucionar estos problemas, una herramienta muy útil es imprimir mensajes de debug en la consola. Esto nos permite ver lo que está sucediendo en cada paso de nuestro código y nos ayuda a identificar el origen del problema.

## Cómo hacerlo
En Kotlin, podemos imprimir mensajes de debug utilizando la función `println()` seguida del mensaje que deseamos imprimir. Por ejemplo:

```Kotlin
println("Iniciando función foo")
```

Esto imprimirá en la consola el mensaje "Iniciando función foo". Además, también podemos utilizar la función `print()` para imprimir el mensaje sin agregar un salto de línea al final.

Otra forma de imprimir mensajes de debug es utilizando la función `Log.d()` de la librería de Android. Esta función nos permite especificar etiquetas para nuestros mensajes y también nos da información sobre la clase, método y línea de código donde se realizó la impresión del mensaje. Un ejemplo de su uso sería:

```Kotlin
Log.d("MainActivity", "Iniciando función foo en la línea 10")
```

## Deep Dive
Es importante tener en cuenta que imprimir demasiados mensajes de debug puede sobrecargar nuestra consola y dificultar la lectura de los resultados. Por eso, es recomendable utilizarlo de forma estratégica, enfocándonos en las partes del código que estamos tratando de solucionar.

También es importante tener en cuenta que los mensajes de debug solo deben utilizarse durante el desarrollo y no deben incluirse en la versión final de la aplicación.

Una forma útil de realizar debugging en Kotlin es utilizar breakpoints en nuestro código y utilizar el modo de debugging en nuestro IDE. Esto nos permite pausar la ejecución del programa en un punto específico y examinar el estado de las variables en ese momento.

## Ver también
- [Debugging en Kotlin](https://kotlinlang.org/docs/tutorials/command-line.html#debugging)
- [Cómo mejorar tus habilidades de debugging en Kotlin](https://blog.kotlin-academy.com/https-medium-com-antonioleiva-how-to-improve-your-debugging-skills-using-kotlin-9d936c91c3f2)
- [Cómo utilizar la función Log en Kotlin](https://developer.android.com/reference/android/util/Log.html)