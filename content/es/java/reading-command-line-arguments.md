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

## Por qué

En Java, los argumentos de línea de comandos son una forma de proporcionar entradas personalizadas al programa durante su ejecución. Esto puede ser útil para realizar tareas específicas o configurar el programa de manera diferente cada vez que se ejecuta.

## Cómo hacerlo

Para leer los argumentos de línea de comandos en Java, primero necesitamos importar la clase `java.lang`.

````java
import java.lang.*;
````
A continuación, podemos utilizar el método `getArgs()` de la clase `String[]` para almacenar todos los argumentos pasados ​​a la línea de comandos en un array.

````java
String[] args = getArgs();
````
Podemos acceder a cada argumento individual utilizando su índice correspondiente en el array `args`. Por ejemplo, si queremos imprimir el primer argumento, usaríamos `args[0]`.

````java
System.out.println(args[0]);
````

## Profundizando

Los argumentos de línea de comandos son una forma de pasar información al programa durante su ejecución. Pueden ser útiles para aceptar entradas de usuario, especificar opciones de configuración o indicar el comportamiento deseado del programa.

Además, los argumentos de línea de comandos pueden ser procesados ​​utilizando la clase `Scanner`, que nos permite leer la entrada del usuario de manera interactiva.

## Ver También

- [Java - Argumentos de línea de comandos](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Java - Clase String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java - Clase Scanner](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)