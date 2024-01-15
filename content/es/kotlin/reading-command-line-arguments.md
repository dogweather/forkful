---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Kotlin: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Por qué

Si bien puede parecer un tema técnico y poco emocionante, saber cómo leer argumentos de línea de comando en Kotlin puede ser extremadamente útil para cualquier desarrollador. Esto te permitirá crear aplicaciones que puedan recibir y procesar distintos parámetros desde la línea de comandos, lo que facilita la interacción con tu programa y lo hace más versátil.

## Cómo hacerlo

Para leer argumentos de línea de comando en Kotlin, simplemente sigue estos pasos:

1. Primero, debes importar la clase `Array`, que nos permitirá almacenar los argumentos que ingresaremos desde la línea de comandos. Puedes hacerlo con el siguiente código:

```Kotlin
import kotlin.Array
```

2. Luego, declara una función `main` como lo haces normalmente en Kotlin, y dentro de ella, crea una variable de tipo `Array<String>` para almacenar los argumentos. Algo así:

```Kotlin
fun main() {
  val args: Array<String> = array()
}
```

3. Dentro de la función `main`, puedes acceder a cada uno de los argumentos ingresados desde la línea de comandos utilizando la variable `args`. Puedes imprimirlos en pantalla para comprobar que están siendo leídos correctamente:

```Kotlin
println(args[0])
println(args[1])
```

4. Ahora, para ejecutar tu programa y pasar los argumentos desde la línea de comandos, simplemente debes escribir `kotlin TuPrograma.kt argumento1 argumento2` en tu terminal. Verás que los argumentos ingresados se imprimirán en pantalla.

¡Y eso es todo! Con estos sencillos pasos, ya puedes leer argumentos de línea de comando en Kotlin.

## Profundizando

Si quieres ir más allá y comprender mejor cómo funciona la lectura de argumentos de línea de comando en Kotlin, aquí te dejo algunos conceptos adicionales.

- Los argumentos ingresados desde la línea de comandos son almacenados en un array, donde el primer elemento (`args[0]`) corresponde al primer argumento ingresado, el segundo elemento (`args[1]`) al segundo argumento ingresado, y así sucesivamente.
- Si quieres acceder a todos los argumentos ingresados de una vez, puedes utilizar el método `joinToString()` de la clase `Array`. Por ejemplo:

```Kotlin
println(args.joinToString())
```
Esto imprimirá todos los argumentos separados por comas.
- Además de poder leer argumentos, también puedes ingresarlos desde la línea de comandos utilizando el método `input()` de la clase `Array`. Por ejemplo:

```Kotlin
val args: Array<String> = Array(2) { input() }
```
Esto te permitirá ingresar dos argumentos en el momento de ejecutar tu programa.

## Ver también

- [Documentación oficial de Kotlin sobre argumentos de línea de comando](https://kotlinlang.org/docs/command-line.html)
- [Cómo utilizar la línea de comandos en Kotlin](https://dev.to/marioa/how-to-use-command-line-arguments-in-kotlin-50g0)
- [Tutorial para leer argumentos de línea de comando en Kotlin](https://www.baeldung.com/kotlin/command-line-arguments)