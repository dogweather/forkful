---
title:                "Kotlin: Leyendo argumentos de línea de comandos."
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# ¿Por qué deberías aprender a leer argumentos de línea de comando?

Si estás comenzando a aprender Kotlin o quieres mejorar tus habilidades de programación, es importante que sepas cómo leer argumentos de línea de comando. Esto te permitirá crear aplicaciones más dinámicas y versátiles, ya que podrás interactuar con el usuario y obtener información de entrada directamente desde la línea de comando.

# Cómo hacerlo en Kotlin

Para leer argumentos de línea de comando en Kotlin, utilizamos la función `args` que viene incorporada en la clase `Array`. Dentro de esta función, podemos acceder a los argumentos ingresados por el usuario utilizando la propiedad `get(index)`, donde `index` representa la posición del argumento en la línea de comando.

Aquí hay un ejemplo de cómo leer un argumento de línea de comando que contiene un número:

```Kotlin
fun main(args: Array<String>) {
    val num = args.get(0).toInt()
    println("El número ingresado es: $num")
}
```

En este ejemplo, estamos leyendo el primer argumento ingresado por el usuario y utilizando la función `toInt()` para convertirlo a un tipo de dato entero. Luego, imprimimos el número en la consola utilizando la función `println()`.

Si ejecutamos este código con el siguiente comando: `kotlin MiPrograma.kt 10`, el resultado sería:

`El número ingresado es: 10`

También podemos utilizar un loop for para recorrer todos los argumentos ingresados por el usuario. Aquí hay un ejemplo:

```Kotlin
fun main(args: Array<String>) {
    for (arg in args) {
        println(arg)
    }
}
```

Este código imprimirá todos los argumentos ingresados por el usuario en líneas separadas.

# Profundizando en la lectura de argumentos de línea de comando

En casos más complejos, podemos utilizar librerías externas como `kotlinx-cli` para leer argumentos de línea de comando de manera más eficiente. Con esta librería, podemos definir argumentos opcionales, valores por defecto, funciones de ayuda, entre otras funciones útiles.

Además, existen otras formas de acceder a argumentos de línea de comando, como utilizando la clase `Scanner` o utilizando la función `System.getenv()` para obtener variables de entorno.

En resumen, aprender a leer argumentos de línea de comando en Kotlin puede mejorar tus habilidades de programación y darte más herramientas para crear aplicaciones más interactivas y dinámicas.

# Ver también

- [Documentación oficial de Kotlin sobre argumentos de línea de comando](https://kotlinlang.org/docs/command-line.html)
- [Tutorial en español de Kotlin sobre lectura de argumentos de línea de comando](https://devexperto.com/lee-argumentos-de-linea-de-comandos-con-kotlin/)
- [Librería kotlinx-cli para leer argumentos de línea de comando en Kotlin](https://github.com/Kotlin/kotlinx-cli)