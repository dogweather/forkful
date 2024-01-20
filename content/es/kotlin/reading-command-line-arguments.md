---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Leer argumentos de línea de comandos se refiere a recibir valores a través de la terminal al inicio de un software. Programamos este patrón ya que es una forma eficaz de pasar datos mientras ejecutamos una aplicación.

## ¿Cómo hacerlo?

Veamos cómo Kotlin facilita la lectura de los argumentos de la línea de comandos en una aplicación de consola.

```Kotlin
fun main(args: Array<String>) {
    args.forEach { arg -> println(arg) }
}
```

Ejecuta el programa con argumentos - `./mi-programa.kt Hola Mundo`. Verás una salida similar:

```
Hola
Mundo
```

## Inmersión profunda

1. **Contexto histórico**: Desde los primeros días de la programación, los argumentos de la línea de comandos han sido una forma efectiva de dar instrucciones a los programas. Kotlin, a pesar de su modernidad, mantiene esta práctica porque es útil y familiar para muchos desarrolladores.

2. **Alternativas**: Hay otras formas de obtener datos al ejecutar un programa, como el uso de archivos de entrada / salida o interacción con el usuario en tiempo real. Sin embargo, los argumentos de la línea de comandos aún se prefieren para ciertos casos de uso puntuales donde la simplicidad es clave.

3. **Detalles de implementación**: Kotlin almacena los argumentos de la línea de comandos en un array de String que se pasa a la función `main()`. Puedes acceder a estos datos como harías con cualquier otro array.

## Ver también

Para explorar este tema en mayor profundidad, consulta estos enlaces útiles:

- [Kotlin Documentation: Command line arguments](https://kotlinlang.org/docs/tutorials/command-line.html)
- [Medium: Kotlin command line arguments](https://medium.com/@cervonefrancesco/kotlin-command-line-arguments-40487f3c5bdb)

Esperamos que este artículo te haya dado una buena visión general de cómo manejar argumentos de la línea de comandos en Kotlin. ¡Feliz codificación!