---
date: 2024-01-26 03:49:55.315463-07:00
description: "C\xF3mo hacerlo: Aqu\xED tienes un peque\xF1o adelanto de c\xF3mo depurar\
  \ en Kotlin con IntelliJ IDEA - el Sherlock Holmes de los IDEs."
lastmod: '2024-03-13T22:44:59.040422-06:00'
model: gpt-4-0125-preview
summary: "Aqu\xED tienes un peque\xF1o adelanto de c\xF3mo depurar en Kotlin con IntelliJ\
  \ IDEA - el Sherlock Holmes de los IDEs."
title: Usando un depurador
weight: 35
---

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
