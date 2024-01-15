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

## Por qué

Imagínate que estás desarrollando una aplicación en Java y te encuentras con un error que no puedes solucionar. ¿Cómo sabes dónde está el problema? Aquí es donde la impresión de salida de depuración (debug output) es útil. Al imprimir información específica en ciertas partes de tu código, puedes identificar mejor dónde está ocurriendo el error y solucionarlo más rápido.

## Cómo hacerlo

La impresión de salida de depuración en Java se realiza utilizando la función ```System.out.println()```, la cual imprime el valor de una variable o una cadena de texto en la consola. Por ejemplo:

```
int num = 5;
System.out.println(num); // imprime "5" en la consola
```
También puedes imprimir múltiples valores separándolos con el signo ```+```. Por ejemplo:

```
String nombre = "María";
int edad = 24;
System.out.println("Mi nombre es " + nombre + " y tengo " + edad + " años."); // imprime "Mi nombre es María y tengo 24 años." en la consola
```

Para imprimir información en un lugar específico de tu código, puedes utilizar comentarios. Por ejemplo:

```
// imprimir el valor actual de la variable "num"
System.out.println("El valor de num es: " + num);
```

Recuerda eliminar los comentarios y las impresiones de salida después de solucionar el error para no afectar el rendimiento de tu aplicación.

## Profundizando

La impresión de salida de depuración es una técnica muy útil para solucionar errores, pero no debe utilizarse como una solución permanente. Es importante aprender a utilizar otras herramientas de depuración para identificar y solucionar problemas en tu código.

También es importante tener en cuenta que la impresión de salida de depuración puede ser una vulnerabilidad de seguridad si se imprime información confidencial, como contraseñas o información personal. Por lo tanto, se recomienda utilizarla solo en entornos de desarrollo y eliminarla antes de lanzar tu aplicación en producción.

## Ver también

- [Java Debugging Tutorial](https://www.baeldung.com/java-debugging)
- [The Power of System.out.println for Debugging](https://blog.frankel.ch/power-system-out-println-debugging/)