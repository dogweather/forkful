---
title:    "Kotlin: Leyendo argumentos de la línea de comandos"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías leer argumentos de línea de comandos?

Si eres un programador de Kotlin, probablemente estés familiarizado con la forma en que tu programa interactúa con el usuario. Sin embargo, hay momentos en los que es posible que necesites que tu programa reciba información directamente desde la línea de comandos. Esto se puede lograr a través de argumentos de línea de comandos, lo que permite al usuario proporcionar información al programa durante su ejecución. Aprender a leer y utilizar estos argumentos de forma efectiva puede mejorar la usabilidad y la funcionalidad de tu programa.

## Cómo leer argumentos de línea de comandos en Kotlin

En Kotlin, existen varias formas de leer argumentos de línea de comandos. Una forma común es a través de la función `main()` y el parámetro `args` que se le pasa. Veamos un ejemplo:

```Kotlin
fun main(args: Array<String>) {
    for (argument in args) { //recorre cada argumento proporcionado
        println(argument) //imprime el argumento en la consola
    }
}
```

Si ejecutamos este programa con la siguiente línea de comandos:

```
$ kotlin MiPrograma.kt argumento1 argumento2
```

La salida sería:

```
argumento1
argumento2
```

También puedes utilizar la clase `Scanner` para leer argumentos de línea de comandos, como se muestra en el siguiente ejemplo:

```Kotlin
fun main() {
    println("Ingresa tu nombre:")
    val scanner = Scanner(System.`in`) //crea un objeto Scanner
    val nombre = scanner.nextLine() //lee el nombre ingresado por el usuario
    println("Hola, $nombre")
}
```

Si ejecutas este programa y escribes tu nombre en la línea de comandos, se imprimirá en la consola un saludo personalizado.

## Profundizando en la lectura de argumentos de línea de comandos en Kotlin

Para obtener una comprensión más profunda de cómo leer y utilizar argumentos de línea de comandos en Kotlin, es importante tener en cuenta que los argumentos se almacenan como cadenas. Esto significa que, si necesitas realizar operaciones matemáticas con los argumentos, primero deberás convertirlos a un tipo de datos numérico.

Además, también puedes acceder a argumentos específicos utilizando su índice en la lista `args`. Por ejemplo, en el primer ejemplo, podríamos imprimir solo el segundo argumento con `println(args[1])`.

## Ver también

- [Documentación oficial de Kotlin sobre argumentos de línea de comandos](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Tutorial de Kotlin en español](https://www.adictosaltrabajo.com/tutoriales/kotlin/)
- [Documentación oficial de Kotlin](https://kotlinlang.org/docs/home.html)