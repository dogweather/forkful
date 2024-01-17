---
title:                "Imprimiendo salida de depuración"
html_title:           "Java: Imprimiendo salida de depuración"
simple_title:         "Imprimiendo salida de depuración"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Imprimir la salida de depuración es una técnica ampliamente utilizada por los programadores para rastrear y solucionar errores en su código. Con él, pueden imprimir mensajes en la consola o en un archivo de registro que les ayuden a comprender qué está sucediendo en su programa y por qué se están produciendo ciertos errores.

## Cómo hacerlo:
Una forma común de imprimir la salida de depuración en Java es utilizar el método `System.out.println()`. Este método toma cualquier objeto como argumento y lo imprime en la consola. Aquí hay un ejemplo:

```Java
int num1 = 5;
int num2 = 10;
System.out.println("La suma de " + num1 + " y " + num2 + " es " + (num1 + num2));
```

Este código imprimirá "La suma de 5 y 10 es 15" en la consola. También se pueden usar expresiones y variables en la cadena para imprimir valores dinámicos.

## Profundizando:
La impresión de la salida de depuración se ha utilizado desde los primeros días de la programación, cuando se usaban impresoras en papel para imprimir mensajes de depuración. En la actualidad, también hay otras formas de depurar código, como usar un depurador integrado en un IDE o escribir pruebas unitarias. Sin embargo, la impresión de la salida de depuración sigue siendo una técnica ampliamente utilizada debido a su simplicidad y flexibilidad.

## Ver también:
- [Debugging with System.out.println() in Java](https://www.geeksforgeeks.org/debugging-in-java-using-system-out-println/)
- [Difference between printing to console and writing to log file](https://stackoverflow.com/questions/5360248/printing-to-the-console-vs-writing-to-a-file/5360295)