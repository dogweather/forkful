---
title:                "Imprimiendo salida de depuración"
aliases:
- /es/kotlin/printing-debug-output.md
date:                  2024-01-20T17:52:51.002504-07:00
model:                 gpt-4-1106-preview
simple_title:         "Imprimiendo salida de depuración"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qué es y por qué?
Imprimir mensajes de depuración es un truco sencillo: mandar información a la consola para entender qué está haciendo tu programa. ¿Por qué? Porque permite seguir la pista de los valores y comportamientos internos del código mientras se ejecuta, facilitando encontrar y arreglar errores.

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
