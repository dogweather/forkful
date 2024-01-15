---
title:                "Escribiendo en el error estándar"
html_title:           "Kotlin: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en la salida estándar de error?

Acostumbramos a escribir en la salida estándar cuando queremos mostrar mensajes al usuario, pero a veces es necesario mostrar mensajes de error. Escribir en la salida estándar de error es una buena práctica ya que nos permite distinguir fácilmente los mensajes de error de los mensajes normales y nos ayuda a identificar y solucionar problemas en nuestro código.

## Cómo hacerlo

Utilizar la salida estándar de error en Kotlin es muy sencillo. Solo se necesita utilizar el objeto `System` y llamar al método `err` para acceder a la salida estándar de error. Luego, se puede utilizar el método `println` para escribir el mensaje de error deseado. Por ejemplo:

```Kotlin
System.err.println("¡Ha ocurrido un error!")
```

Esto imprimirá en la consola el mensaje "¡Ha ocurrido un error!" en rojo, lo que lo distingue claramente de los mensajes normales en la salida estándar.

## Profundizando

Al utilizar la salida estándar de error, también es posible especificar el tipo de error que se ha producido utilizando el método `forErr`. Por ejemplo, si se produce un `IOException`, se puede escribir:

```Kotlin
System.err.forErr(IOException::class.java).println("¡Ha ocurrido un error de entrada y salida!")
```

Esto imprimirá el mensaje con la etiqueta "[IOException]" al principio, lo que facilita la identificación del tipo de error. Además, se pueden utilizar otros métodos como `printf` en lugar de `println` para formatear el mensaje de error de manera más específica, utilizando argumentos como en el conocido método `printf` de Java.

## Ver también

- [Documentación oficial de Kotlin sobre la salida estándar de error](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-output-stream/err.html)
- [Artículo sobre la diferencia entre la salida estándar y la salida estándar de error](https://www.pluralsight.com/guides/kotlin-error-handling-stdout-stderr-difference-and-examples)