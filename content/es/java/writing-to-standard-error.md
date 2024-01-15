---
title:                "Escribiendo en el error estándar"
html_title:           "Java: Escribiendo en el error estándar"
simple_title:         "Escribiendo en el error estándar"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir en la salida estándar de error?

Escribir en la salida estándar de error es una técnica común en la programación en Java. Es una manera de mostrar mensajes de error o advertencia al usuario durante la ejecución del programa. Esto permite una mejor gestión de errores y ayuda a mejorar la calidad del código.

## Cómo hacerlo

Para escribir en la salida estándar de error en Java, se utiliza el objeto System.err. A continuación se muestra un ejemplo de cómo imprimir un mensaje de error en la consola:

```Java
System.err.println("¡Error! Algo salió mal.");
```

El resultado de este código sería:

```
¡Error! Algo salió mal.
```

Además de imprimir un mensaje de error, también se pueden escribir otros tipos de datos en la salida estándar de error, como números o caracteres. Todo lo que se imprima utilizando el objeto System.err se mostrará en la consola con un color rojo, lo que ayuda a identificar fácilmente los mensajes de error en medio de la salida estándar de la ejecución del programa.

## Profundizando

Hay diferentes escenarios en los que escribir en la salida estándar de error puede ser útil. Por ejemplo, si se está trabajando en un proyecto en equipo y se desea colaborar con otros programadores, escribir en la salida estándar de error puede ser una manera de comunicar problemas o advertencias de manera clara y concisa. También es útil cuando se están realizando pruebas de código y se necesita ver cuando ocurre un error específico.

Además de imprimir mensajes de error en la salida estándar de error, también es posible redirigir esta salida a un archivo de texto. Esto puede ser útil para guardar registros o para informar sobre errores en un archivo específico en lugar de imprimirlos en la consola.

## Ver también

- [Java documentation on System.err](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#err)
- [Tutorial on Standard I/O in Java](https://www.tutorialspoint.com/java/java_system_class.htm)
- [Stack Overflow thread discussing the use of System.err](https://stackoverflow.com/questions/8366080/difference-between-system-out-println-and-system-err-println-in-java)