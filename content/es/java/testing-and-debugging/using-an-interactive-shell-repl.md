---
date: 2024-01-26 04:15:25.695092-07:00
description: "C\xF3mo hacerlo: Iniciar un REPL en Java es simple con la herramienta\
  \ `jshell` introducida en Java 9. As\xED es como puedes empezar y abrir una sesi\xF3\
  n b\xE1sica."
lastmod: '2024-03-13T22:44:58.939161-06:00'
model: gpt-4-0125-preview
summary: Iniciar un REPL en Java es simple con la herramienta `jshell` introducida
  en Java 9.
title: Usando una shell interactiva (REPL)
weight: 34
---

## Cómo hacerlo:
Iniciar un REPL en Java es simple con la herramienta `jshell` introducida en Java 9. Así es como puedes empezar y abrir una sesión básica:

```Java
jshell> int sum(int a, int b) {
    ...> return a + b;
    ...> }
|  method sum(int,int) created

jshell> sum(5, 7)
$1 ==> 12
```

Sal de cualquier momento con `/exit`.

```Java
jshell> /exit
|  Adiós
```

## Profundización
Antes de `jshell`, los programadores de Java no tenían un REPL oficial, a diferencia de los desarrolladores de Python o Ruby. Utilizaban IDEs o escribían programas completos incluso para tareas triviales. `jshell` fue un cambio de juego a partir de Java 9, cerrando esa brecha.

Las alternativas incluyen compiladores en línea o complementos de IDE, pero no igualan la inmediatez de `jshell`. En cuanto a los internos, `jshell` utiliza la API del Compilador de Java para ejecutar fragmentos de código, lo cual es bastante ordenado. Es más que un patio de juegos: puede importar librerías, definir clases y más. Esto lo convierte en una herramienta robusta para prototipos.

## Ver También
- [Guía del Usuario de JShell](https://docs.oracle.com/javase/9/jshell/introduction-jshell.htm)
- [Referencia de Herramientas de la Plataforma Java, Edición Estándar](https://docs.oracle.com/javase/9/tools/tools-and-command-reference.htm#JSWOR719)
- [API del Compilador de Java](https://docs.oracle.com/javase/9/docs/api/javax/tools/JavaCompiler.html)
