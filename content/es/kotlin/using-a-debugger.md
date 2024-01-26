---
title:                "Usando un depurador"
date:                  2024-01-26T03:49:55.315463-07:00
model:                 gpt-4-0125-preview
simple_title:         "Usando un depurador"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/using-a-debugger.md"
---

{{< edit_this_page >}}

## Qué y Por Qué
Sumergirse en un depurador trata sobre avanzar paso a paso por tu código, observando cómo funcionan los engranajes y atrapando esos molestos errores con las manos en la masa. Los programadores usan depuradores porque son las herramientas de detective que nos ayudan a descubrir dónde están los errores sin arrancarnos el cabello.

## Cómo hacerlo:
Aquí tienes un pequeño adelanto de cómo depurar en Kotlin con IntelliJ IDEA - el Sherlock Holmes de los IDEs:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("Adivina el número: ")
        guess = readLine()?.toIntOrNull() ?: continue // Ignorar entradas incorrectas

        // Establece un punto de interrupción aquí para observar 'guess' en acción
        if (guess < mysteryNumber) {
            println("¡Demasiado bajo!")
        } else if (guess > mysteryNumber) {
            println("¡Demasiado alto!")
        }
    }

    println("¡Lo has conseguido! El número misterioso era $mysteryNumber")
}
```

Salida del depurador:
```
Adivina el número: 
10
¡Demasiado bajo!
Adivina el número: 
50
¡Demasiado alto!
Adivina el número:
42
¡Lo has conseguido! El número misterioso era 42
```

## Inmersión Profunda
Los depuradores están en el juego desde los años 50. En aquel entonces, eran bastante primitivos, y depurar podría ser más acerca del hardware que del software. Hoy en día, un depurador como el de IntelliJ IDEA nos permite establecer puntos de interrupción, avanzar por el código línea por línea e inspeccionar el estado de las variables a nuestro antojo.

Aunque el depurador de IntelliJ es súper útil para Kotlin, no es el único pez en el mar. Hay una gama de alternativas como Logcat para el desarrollo de Android, o herramientas de línea de comandos como jdb para los minimalistas. La magia detrás de escena aquí trata principalmente sobre la Interfaz de Herramientas de la Máquina Virtual de Java (JVMTI), que permite a los depuradores interactuar con la Máquina Virtual de Java, manteniendo a los desarrolladores de Kotlin en el bucle.

## Ver También
- Documentación del depurador de IntelliJ IDEA: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
- Documentación de Kotlin sobre Depuración: [https://kotlinlang.org/docs/debugging.html](https://kotlinlang.org/docs/debugging.html)
- Las raíces de la depuración en la historia: [http://history-computer.com/Internet/Maturing/Debugging.html](http://history-computer.com/Internet/Maturing/Debugging.html)