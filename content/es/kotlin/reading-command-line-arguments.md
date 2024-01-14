---
title:                "Kotlin: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si eres un programador de Kotlin, probablemente estés familiarizado con el concepto de argumentos de línea de comando. Pero si eres nuevo en el mundo de la programación o estás buscando aprender más sobre Kotlin, es importante entender por qué es importante saber cómo leer estos argumentos de línea de comando.

Los argumentos de línea de comando son una forma de pasar información a un programa en tiempo de ejecución. Son especialmente útiles cuando se quiere que un programa realice diferentes acciones dependiendo de la entrada del usuario. Sin saber cómo leer y utilizar estos argumentos, tu programa no podrá interactuar de manera efectiva con el usuario.

## Cómo hacerlo

Si ya has trabajado un poco con Kotlin, probablemente conozcas la función main() que se utiliza para iniciar un programa. Dentro de esta función, podemos utilizar el parámetro args de tipo Array<String> para leer los argumentos de línea de comando.

Veamos un ejemplo práctico:

```Kotlin
fun main(args: Array<String>) {
   println("Hola, ${args[0]}!")
}
```

En este código, estamos utilizando la función println() para imprimir un saludo personalizado con el primer argumento de línea de comando que ingrese el usuario. Si ejecutamos este programa con el comando "kotlin HolaMundo.kt mi nombre", la salida sería "Hola, mi nombre!".

Pero, ¿qué pasa si queremos leer más de un argumento de línea de comando? Podemos hacerlo simplemente utilizando el índice correspondiente del argumento en el array. Por ejemplo:

```Kotlin
fun main(args: Array<String>) {
   println("Hola, ${args[0]} y ${args[1]}!")
}
```

En este caso, si ejecutamos el mismo comando que antes, la salida sería "Hola, mi nombre y Kotlin!". También podemos utilizar un bucle for para imprimir todos los argumentos a la vez.

En resumen, para leer los argumentos de línea de comando en Kotlin, simplemente necesitas utilizar el parámetro args en la función main() y acceder a los elementos del array utilizando los índices correspondientes.

## Profundizando

Ahora que sabes cómo leer los argumentos de línea de comando, es importante entender cómo se almacenan y se pasan estos argumentos. En Kotlin, los argumentos de línea de comando se almacenan en un array de tipo String, donde cada elemento corresponde a un argumento ingresado por el usuario.

Además, es importante tener en cuenta que los argumentos de línea de comando se pasan en orden y pueden contener espacios, por lo que es necesario utilizar las comillas para delimitar argumentos con más de una palabra.

## Ver también

- [Documentación oficial de Kotlin sobre argumentos de línea de comando](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Tutorial sobre argumentos de línea de comando en Kotlin de Programar en Java](https://programarenjava.com/tutorial-argumentos-de-linea-de-comando-en-kotlin/)