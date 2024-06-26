---
date: 2024-01-20 17:52:51.002504-07:00
description: "C\xF3mo se hace: Salida de ejemplo cuando `debugMode` est\xE1 activo."
lastmod: '2024-04-05T21:54:00.381337-06:00'
model: gpt-4-1106-preview
summary: "Salida de ejemplo cuando `debugMode` est\xE1 activo."
title: "Imprimiendo salida de depuraci\xF3n"
weight: 33
---

## Cómo se hace:
```Kotlin
fun main() {
    val debugMode = true // Activa o desactiva la depuración

    if (debugMode) {
        println("Iniciando el programa...")
    }

    val resultado = suma(5, 3)

    if (debugMode) {
        println("Resultado de la suma: $resultado")
    }
}

fun suma(a: Int, b: Int): Int {
    return a + b
}
```
Salida de ejemplo cuando `debugMode` está activo:
```
Iniciando el programa...
Resultado de la suma: 8
```

## Inmersión profunda:
Históricamente, los desarrolladores necesitaban una forma de entender qué estaba pasando "dentro" de un programa. El método más rudimentario es imprimir mensajes en la consola. Claro, hay alternativas más sofisticadas como los depuradores y herramientas de logging que categorizan los mensajes (info, error, etc.), pero un `println` rápido siempre es útil.

Kotlin, siendo un lenguaje moderno, no reinventó la rueda con esto: se usa la función `println` igual que en otros lenguajes como Java. Pero para controlar los mensajes de depuración sin tener que eliminarlos manualmente, puedes usar una variable como `debugMode`. Así, activando o desactivando esta variable, controlas lo que se imprime.

Otro detalle es la interpolación de strings en Kotlin, que permite insertar variables o expresiones directamente en la cadena de texto que vas a imprimir, haciendo más legible y conciso el código.

## Ver también:
- Documentación oficial de Kotlin sobre cómo manejar la entrada/salida: [kotlinlang.org/docs/reference/basic-syntax.html#using-variables](https://kotlinlang.org/docs/reference/basic-syntax.html#using-variables)
- Kotlin logging frameworks para un manejo avanzado de mensajes de depuración: [github.com/MicroUtils/kotlin-logging](https://github.com/MicroUtils/kotlin-logging)
- Tutorial sobre depuración efectiva en Kotlin: [raywenderlich.com/4736-android-debugging-with-kotlin](https://www.raywenderlich.com/4736-android-debugging-with-kotlin)
