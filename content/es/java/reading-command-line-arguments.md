---
title:                "Lectura de argumentos de línea de comandos"
date:                  2024-01-20T17:56:05.146473-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lectura de argumentos de línea de comandos"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Leer argumentos de la línea de comandos significa acceder a los datos que el usuario proporciona al ejecutar un programa. Los programadores lo hacen para personalizar la ejecución sin cambiar el código; dinamismo en estado puro.

## Cómo hacerlo:
```java
public class Saludador {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Hola, " + args[0] + "!");
        } else {
            System.out.println("Hola, ¿quién eres?");
        }
    }
}
```
Si compilas y ejecutas con `java Saludador Carlos`, la salida será `Hola, Carlos!`. Sin argumentos, la salida es `Hola, ¿quién eres?`.

## Inmersión Profunda
En el pasado, los argumentos de la línea de comandos eran la norma, especialmente en sistemas operativos tipo Unix. Hoy, las GUI son comunes, pero la línea de comandos sigue siendo crucial para scripts y automatización.

Las alternativas incluyen leer archivos de configuración o usar variables de entorno, pero los argumentos de línea de comandos proporcionan una forma rápida y sin estado de pasar información.

Técnicamente, `args` es un array de `String`, donde cada elemento es un argumento proporcionado. La posición importa: `args[0]` es el primer argumento, `args[1]` el segundo, y así sucesivamente.

## Ver También
- [Documentación oficial de Oracle sobre la línea de comandos](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorial de Apache Commons CLI para manejar argumentos de línea de comandos complejos](https://commons.apache.org/proper/commons-cli/)
- [Guía para principiantes sobre Java](https://www.learnjavaonline.org/)