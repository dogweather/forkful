---
title:    "Java: Generación de números aleatorios"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/java/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Por qué generar números aleatorios en Java

Generar números aleatorios puede ser una tarea útil en varios escenarios de programación. Ya sea para crear datos de prueba, mejorar la seguridad o darle un toque de aleatoriedad a tu programa, aprender a generar números aleatorios en Java puede ser una habilidad valiosa para cualquier programador.

## Cómo generar números aleatorios en Java

Para generar números aleatorios en Java, utilizamos la clase `Random` que viene incluida en el paquete `java.util`. Primero, debemos importar esta clase a nuestro archivo Java:

```Java
import java.util.Random;
```

A continuación, creamos una instancia de la clase `Random` y utilizamos sus métodos para generar números aleatorios como se muestra en el siguiente ejemplo:

```Java
// Crear una instancia de la clase Random
Random random = new Random();
// Generar un número aleatorio entero entre 0 y 10
int x = random.nextInt(11);
// Generar un número aleatorio double entre 0 y 1
double y = random.nextDouble();
// Generar un número aleatorio boolean
boolean z = random.nextBoolean();

// Imprimir los números generados
System.out.println(x);
System.out.println(y);
System.out.println(z);
```

La salida de este código puede variar cada vez que se ejecute, mostrando diferentes valores aleatorios.

## Profundizando en la generación de números aleatorios en Java

Aunque la clase `Random` es la más comúnmente utilizada para generar números aleatorios en Java, también existen otras opciones como la clase `Math` y la interfaz `ThreadLocalRandom`. La clase `Math` tiene métodos estáticos para generar números aleatorios, como `random()` que genera un número double entre 0 y 1. La interfaz `ThreadLocalRandom` permite generar números aleatorios en hilos de ejecución.

Es importante recordar que la generación de números aleatorios no es verdaderamente aleatoria, sino pseudoaleatoria, lo que significa que los números aparentan ser aleatorios, pero realmente siguen un patrón determinado. Por lo tanto, no deben utilizarse para propósitos criptográficos o de seguridad.

## Ver también
- [Documentación de la clase Random en Java](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Tutorial de Java: Generación de números aleatorios](https://www.javatpoint.com/generating-random-number-in-java)
- [Guía de buenas prácticas en la generación de números aleatorios en Java](http://c2.com/cgi/wiki?GeneratingRandomNumbersInJava)