---
title:                "Leyendo argumentos de línea de comandos"
html_title:           "Java: Leyendo argumentos de línea de comandos"
simple_title:         "Leyendo argumentos de línea de comandos"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

¡Hola lectores de Java!

En este artículo, hablaremos sobre cómo leer argumentos de línea de comando en Java. Así es, esa pequeña (pero útil) parte de su programa que le permite recibir entrada del usuario directamente desde la línea de comando.

## ¿Qué y por qué?

Leer argumentos de línea de comando en Java simplemente significa permitir que su programa reciba entradas directamente desde la línea de comando. Los programadores hacen esto para hacer que su código sea más interactivo, ya que permite que los usuarios ingresen información mientras el programa se está ejecutando.

## ¿Cómo hacerlo?

Veamos un ejemplo de código para mostrar cómo leer argumentos de línea de comando en Java:
```Java
public class CommandLineArguments {

  public static void main(String[] args) {
    // Primer argumento (índice 0) es el nombre del programa
    System.out.println("Argumento 1: " + args[0]);
    
    // Segundo argumento (índice 1) es el primer argumento ingresado por el usuario
    System.out.println("Argumento 2: " + args[1]);
    
    // Y así sucesivamente
    System.out.println("Argumento 3: " + args[2]);
  }
}
```
Y aquí está cómo se vería la salida cuando se ejecuta este programa desde la línea de comando:
```
$ java CommandLineArguments primero segundo tercero
Argumento 1: primero
Argumento 2: segundo
Argumento 3: tercero
```

## Profundizando

Ahora, un dato interesante: los argumentos de línea de comando en Java no siempre han sido compatibles. Antes de Java 5, los programadores tenían que usar una biblioteca externa o escribir su propio código para leer los argumentos de línea de comando.

Una alternativa para leer argumentos de línea de comando en Java es usar la clase Scanner de la biblioteca estándar para recibir entrada del usuario. Sin embargo, leer argumentos de línea de comando es más eficiente ya que evita las operaciones innecesarias de conversión de tipo.

La implementación de la lectura de argumentos de línea de comando en Java se basa en la clase StringBuffer, que usa un búfer para almacenar la entrada de usuario antes de convertirla en tipos de datos adecuados.

## Ver también

Para obtener más información sobre la lectura de argumentos de línea de comando en Java, puede consultar la documentación oficial de Oracle: https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html

¡Eso es todo por hoy! Esperamos que este artículo haya sido útil y que ahora tengas una mejor comprensión de cómo leer argumentos de línea de comando en Java. ¡Hasta la próxima vez!