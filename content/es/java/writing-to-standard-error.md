---
title:                "Java: Escribir en la salida de error estándar"
simple_title:         "Escribir en la salida de error estándar"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en standard error en Java?

Escribir en standard error en Java es una práctica común en la programación, ya que permite que los desarrolladores puedan detectar y solucionar errores de manera más eficiente. Esto permite que el código sea más robusto y confiable, lo cual es esencial en el mundo de la programación.

## Cómo hacerlo

Para escribir en standard error en Java, primero debemos importar la clase "System". Luego, podemos usar el método "err" de la clase "System" para imprimir nuestro mensaje o error. Veamos un ejemplo:

```Java
import java.lang.System;

System.err.println("¡Este es un mensaje de error!");
```

Al ejecutar este código, veremos el siguiente resultado en nuestra consola:

```
¡Este es un mensaje de error!
```

Podemos notar que este mensaje aparece en rojo, lo cual facilita su identificación y nos ayuda a encontrar rápidamente cualquier error en nuestro código.

Es importante mencionar que al igual que con la impresión en standard output (con el método "out" de la clase "System"), también podemos usar el método "print" o "println" para imprimir en standard error:

```Java
System.err.print("Este es un mensaje");
System.err.println(" de error");
```

El resultado sería el mismo que en el ejemplo anterior.

## Profundizando en la escritura en standard error

Al igual que en standard output, es posible redirigir la salida de standard error hacia un archivo o cualquier otro lugar que deseemos. También podemos utilizar otros métodos de la clase "System.err", como "format" para dar formato a nuestros mensajes de error o "flush" para limpiar el buffer antes de imprimir.

Otra ventaja de la escritura en standard error es que podemos combinarla con la captura de excepciones, lo que nos permite manejar mejor los errores en nuestro código.

En resumen, escribir en standard error en Java es una práctica importante y útil que nos ayuda a mejorar la calidad de nuestro código y a solucionar errores de manera más eficiente.

## Ver también

- [Cómo imprimir en consola en Java](https://www.codingame.com/playgrounds/24675/como-imprimir-en-consola-en-java)
- [Documentación oficial de la clase System](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Tutorial de manejo de excepciones en Java](https://www.javatpoint.com/exception-handling-in-java)