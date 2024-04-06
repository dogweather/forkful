---
date: 2024-01-20 17:56:05.146473-07:00
description: "C\xF3mo hacerlo: Si compilas y ejecutas con `java Saludador Carlos`,\
  \ la salida ser\xE1 `Hola, Carlos!`. Sin argumentos, la salida es `Hola, \xBFqui\xE9\
  n eres?`."
lastmod: '2024-04-05T21:54:00.305630-06:00'
model: gpt-4-1106-preview
summary: "Si compilas y ejecutas con `java Saludador Carlos`, la salida ser\xE1 `Hola,\
  \ Carlos!`."
title: "Lectura de argumentos de l\xEDnea de comandos"
weight: 23
---

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
