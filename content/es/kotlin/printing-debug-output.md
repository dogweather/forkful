---
title:                "Impresión de salida de depuración"
html_title:           "Kotlin: Impresión de salida de depuración"
simple_title:         "Impresión de salida de depuración"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Por qué?

A veces, mientras desarrollamos una aplicación en Kotlin, nos encontramos con errores o problemas en nuestro código y es importante poder identificar dónde se está produciendo el error y por qué. En esos casos, la impresión de salida de depuración (debug output) puede ser una herramienta invaluable para ayudarnos a comprender la ejecución de nuestro código y encontrar una solución.

## Cómo hacerlo

La impresión de salida de depuración en Kotlin se logra utilizando la función `println()` o la función `Log.d()` si estás desarrollando una aplicación para Android. Ambas funciones pueden imprimir mensajes de texto o variables en la consola del sistema, lo que nos permite ver la información mientras se ejecuta el código.

```Kotlin
val nombre = "Juan"
println("¡Hola, $nombre!") // salida: ¡Hola, Juan!

val edad = 25
Log.d("Edad", "$edad años") // salida en consola del sistema: Edad: 25 años
```

También podemos utilizar la función `print()` si solo queremos imprimir un mensaje simple en la misma línea.

```Kotlin
print("Hello")
print("World") // salida: HelloWorld
```

Si queremos imprimir una variable que no sea de tipo `String`, podemos utilizar la función `toString()` para convertirla en una cadena de texto.

```Kotlin
val numero = 42
println("El número es: " + numero.toString()) // salida: El número es: 42
```

Además de imprimir mensajes y variables, también podemos imprimir información de depuración en diferentes niveles utilizando la función `Log`.

```Kotlin
Log.d("DEBUG", "El valor de la variable es: $variable") // información de depuración
Log.i("INFORMACIÓN", "Operación completada exitosamente") // información general
Log.e("ERROR", "No se puede realizar la operación") // error crítico
```

También podemos dar formato a nuestros mensajes de salida utilizando la cadena de formato `%`.

```Kotlin
val numero1 = 5
val numero1 = 10
println("La suma de %d y %d es %d", numero1, numero2, numero1 + numero2) // salida: La suma de 5 y 10 es 15
```

## Más a fondo

Aunque imprimir salida de depuración puede ser útil en diferentes ocasiones, es importante recordar eliminarla una vez que hayamos encontrado y solucionado el error en nuestro código. Dejar mensajes de salida de depuración innecesarios puede afectar el rendimiento de nuestra aplicación.

También es importante tener en cuenta que muchos sistemas tienen sus propias funciones y métodos para imprimir salida de depuración, como `Log` en Android o `NSLog` en iOS. Asegúrate de investigar sobre las herramientas disponibles en el sistema que estés utilizando para imprimir salida de depuración de manera más efectiva.

## Ver También

- [Documentación oficial de Kotlin](https://kotlinlang.org/docs/home.html)
- [Usando la función Log en Android](https://developer.android.com/reference/android/util/Log)