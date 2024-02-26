---
date: 2024-01-26 04:15:25.695092-07:00
description: "Un REPL (Bucle de Leer-Evaluar-Imprimir) es una consola interactiva\
  \ que procesa entradas individuales del usuario, ejecuta c\xF3digo y devuelve el\
  \ resultado.\u2026"
lastmod: '2024-02-25T18:49:55.428189-07:00'
model: gpt-4-0125-preview
summary: "Un REPL (Bucle de Leer-Evaluar-Imprimir) es una consola interactiva que\
  \ procesa entradas individuales del usuario, ejecuta c\xF3digo y devuelve el resultado.\u2026"
title: Usando una shell interactiva (REPL)
---

{{< edit_this_page >}}

## Qué y Por Qué?
Un REPL (Bucle de Leer-Evaluar-Imprimir) es una consola interactiva que procesa entradas individuales del usuario, ejecuta código y devuelve el resultado. Los programadores lo utilizan para experimentos rápidos, depuración o aprendizaje, ya que permite obtener comentarios inmediatos e iterar.

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
